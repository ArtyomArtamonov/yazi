use std::{collections::{BTreeMap, HashSet}, path::PathBuf};

use tracing::trace;

use super::{Scheduler, TASKS_PADDING, TASKS_PERCENT};
use crate::{config::OPEN, misc::tty_size};

#[derive(Clone, Debug)]
pub struct Task {
	pub id:    usize,
	pub name:  String,
	pub stage: TaskStage,

	pub found:     u32,
	pub processed: u32,

	pub todo: u64,
	pub done: u64,
}

impl Task {
	pub fn new(id: usize, name: String) -> Self {
		Self { id, name, stage: Default::default(), found: 0, processed: 0, todo: 0, done: 0 }
	}
}

#[derive(Debug)]
pub enum TaskOp {
	// task_id, size
	New(usize, u64),
	// task_id, processed, size
	Adv(usize, u32, u64),
	// task_id
	Done(usize),
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub enum TaskStage {
	#[default]
	Pending,
	Dispatched,
	Hooked,
}

pub struct Tasks {
	scheduler: Scheduler,

	pub visible:  bool,
	pub cursor:   usize,
	pub progress: (u8, u32),
}

impl Tasks {
	pub fn start() -> Self {
		Self { scheduler: Scheduler::start(), visible: false, cursor: 0, progress: (100, 0) }
	}

	#[inline]
	pub fn limit() -> usize {
		(tty_size().ws_row * TASKS_PERCENT / 100).saturating_sub(TASKS_PADDING) as usize
	}

	pub fn toggle(&mut self) -> bool {
		self.visible = !self.visible;
		true
	}

	pub fn next(&mut self) -> bool {
		let limit = Self::limit().min(self.scheduler.running.read().len());

		let old = self.cursor;
		self.cursor = limit.saturating_sub(1).min(self.cursor + 1);

		old != self.cursor
	}

	pub fn prev(&mut self) -> bool {
		let old = self.cursor;
		self.cursor = self.cursor.saturating_sub(1);
		old != self.cursor
	}

	pub fn paginate(&self) -> Vec<Task> {
		let running = self.scheduler.running.read();
		running.values().take(Self::limit()).cloned().collect::<Vec<_>>()
	}

	pub fn cancel(&self) -> bool {
		let id = self.scheduler.running.read().values().skip(self.cursor).next().map(|t| t.id);
		id.map(|id| self.scheduler.cancel(id)).unwrap_or(false)
	}

	pub fn file_open(&self, targets: Vec<(PathBuf, String)>) -> bool {
		let mut openers = BTreeMap::new();
		for target in targets {
			if let Some(opener) = OPEN.opener(&target.0, &target.1) {
				openers
					.entry(opener.clone())
					.or_insert_with(|| vec![])
					.push(target.0.to_string_lossy().into_owned());
			}
		}
		for (opener, args) in openers {
			if opener.spread {
				self.scheduler.process_open(&opener, &args);
				continue;
			}
			for target in args {
				self.scheduler.process_open(&opener, &[target]);
			}
		}
		false
	}

	pub fn file_cut(&self, src: &HashSet<PathBuf>, dest: PathBuf, force: bool) -> bool {
		for p in src {
			let to = dest.join(p.file_name().unwrap());
			if force && *p == to {
				trace!("file_cut: same file, skipping {:?}", to);
			} else {
				self.scheduler.file_cut(p.clone(), to, force);
			}
		}
		false
	}

	pub fn file_copy(
		&self,
		src: &HashSet<PathBuf>,
		dest: PathBuf,
		force: bool,
		follow: bool,
	) -> bool {
		for p in src {
			let to = dest.join(p.file_name().unwrap());
			if force && *p == to {
				trace!("file_copy: same file, skipping {:?}", to);
			} else {
				self.scheduler.file_copy(p.clone(), to, force, follow);
			}
		}
		false
	}

	pub fn file_remove(&self, targets: Vec<PathBuf>, permanently: bool) -> bool {
		for p in targets {
			if permanently {
				self.scheduler.file_delete(p);
			} else {
				self.scheduler.file_trash(p);
			}
		}
		false
	}

	pub fn update_progress(&mut self, percent: u8, left: u32) -> bool {
		if self.progress.0 == percent {
			return false;
		}

		self.progress = (percent, left);
		true
	}
}