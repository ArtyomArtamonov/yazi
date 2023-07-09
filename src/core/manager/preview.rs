use std::{fs::File, io::BufReader, path::{Path, PathBuf}, sync::OnceLock};

use adapter::kitty::Kitty;
use anyhow::{anyhow, Context, Result};
use image::imageops::FilterType;
use ratatui::layout::Rect;
use syntect::{easy::HighlightLines, highlighting::{Theme, ThemeSet}, parsing::SyntaxSet, util::as_24_bit_terminal_escaped};
use tokio::{fs, task::JoinHandle};

use super::{Folder, ALL_RATIO, PREVIEW_BORDER, PREVIEW_PADDING, PREVIEW_RATIO};
use crate::{config::{PREVIEW, THEME}, core::{adapter, Precache}, emit, misc::{first_n_lines, tty_ratio, tty_size}};

static SYNTECT_SYNTAX: OnceLock<SyntaxSet> = OnceLock::new();
static SYNTECT_THEME: OnceLock<Theme> = OnceLock::new();

#[derive(Debug)]
pub struct Preview {
	pub path: PathBuf,
	pub data: PreviewData,
	handle:   Option<JoinHandle<()>>,
}

#[derive(Debug, Default)]
pub enum PreviewData {
	#[default]
	None,
	Folder,
	Text(String),
	Image(Vec<u8>),
}

impl Preview {
	pub fn new() -> Self {
		Self { path: Default::default(), data: Default::default(), handle: Default::default() }
	}

	fn size() -> (u16, u16) {
		let s = tty_size();
		let col = (s.ws_col as u32 * PREVIEW_RATIO / ALL_RATIO) as u16;
		(col.saturating_sub(PREVIEW_BORDER), s.ws_row.saturating_sub(PREVIEW_PADDING))
	}

	pub fn go(&mut self, path: &Path, mime: &str) {
		if let Some(handle) = self.handle.take() {
			handle.abort();
		}

		let (path, mime) = (path.to_path_buf(), mime.to_string());
		self.handle = Some(tokio::spawn(async move {
			let result = if mime == "inode/directory" {
				Self::folder(&path).await
			} else if mime == "application/json" {
				Self::json(&path).await.map(PreviewData::Text)
			} else if mime.starts_with("text/") || mime.ends_with("/xml") {
				Self::highlight(&path).await.map(PreviewData::Text)
			} else if mime.starts_with("image/") {
				Self::image(&path).await.map(PreviewData::Image)
			} else if mime.starts_with("video/") {
				Self::video(&path).await.map(PreviewData::Image)
			} else {
				Err(anyhow!("Unsupported mimetype: {}", mime))
			};

			emit!(Preview(path, result.unwrap_or_default()));
		}));
	}

	pub fn reset(&mut self) -> bool {
		if self.path == PathBuf::default() {
			return false;
		}

		self.path = Default::default();
		self.data = Default::default();
		true
	}

	pub async fn folder(path: &Path) -> Result<PreviewData> {
		Folder::read(&path).await;
		Ok(PreviewData::Folder)
	}

	pub async fn image(mut path: &Path) -> Result<Vec<u8>> {
		let cache = Precache::cache(path);
		if cache.exists() {
			path = cache.as_path();
		}

		let (w, h) = {
			let r = tty_ratio();
			let (w, h) = Self::size();
			let (w, h) = ((w as f64 * r.0) as u32, (h as f64 * r.1) as u32);
			(w.min(PREVIEW.max_width), h.min(PREVIEW.max_height))
		};

		let file = fs::read(path).await?;
		tokio::task::spawn_blocking(move || -> Result<Vec<u8>> {
			let img = image::load_from_memory(&file)?;
			Kitty::image_show(if img.width() > w || img.height() > h {
				img.resize(w, h, FilterType::Triangle)
			} else {
				img
			})
		})
		.await?
	}

	pub async fn video(path: &Path) -> Result<Vec<u8>> {
		Precache::video(path).await?;

		let cache = Precache::cache(path);
		Self::image(&cache).await
	}

	pub async fn json(path: &Path) -> Result<String> {
		Ok(
			Precache::json(path)
				.await?
				.lines()
				.take(Self::size().1 as usize)
				.collect::<Vec<_>>()
				.join("\n"),
		)
	}

	pub async fn highlight(path: &Path) -> Result<String> {
		let syntax = SYNTECT_SYNTAX.get_or_init(|| SyntaxSet::load_defaults_newlines());

		let theme = SYNTECT_THEME.get_or_init(|| {
			let from_file = || -> Result<Theme> {
				let file = File::open(&THEME.syntect.theme)?;
				Ok(ThemeSet::load_from_reader(&mut BufReader::new(file))?)
			};
			from_file().unwrap_or_else(|_| ThemeSet::load_defaults().themes["base16-ocean.dark"].clone())
		});

		let ext = path.extension().context("no extension found")?.to_string_lossy().to_string();

		let lines = first_n_lines(path, Self::size().1 as usize).await?;

		tokio::task::spawn_blocking(move || -> Result<String> {
			let mut buf = "".to_string();
			if let Some(syn) = syntax.find_syntax_by_extension(&ext) {
				let mut h = HighlightLines::new(syn, theme);
				let tab = " ".repeat(PREVIEW.tab_size as usize);
				for line in lines {
					let line = line.replace('\t', &tab);
					let ranges = h.highlight_line(&line, &syntax)?;
					buf.push_str(&as_24_bit_terminal_escaped(&ranges, false));
					buf.push('\n');
				}
			}
			Ok(buf)
		})
		.await?
	}
}