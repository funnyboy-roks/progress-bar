use std::{
    fmt::Display,
    io::Write,
    sync::{atomic::AtomicUsize, Arc},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProgressValue {
    numerator: u64,
    denominator: u64,
}

impl From<f64> for ProgressValue {
    fn from(value: f64) -> Self {
        // This doesn't need to be too large as we're limited to the width of the progress bar
        // anyways
        let precision = 1_000;
        Self {
            numerator: (value * precision as f64).round() as u64,
            denominator: precision,
        }
    }
}
impl From<f32> for ProgressValue {
    fn from(value: f32) -> Self {
        // This doesn't need to be too large as we're limited to the width of the progress bar
        // anyways
        let precision = 1_000;
        Self {
            numerator: (value * precision as f32).round() as u64,
            denominator: precision,
        }
    }
}
pub trait Progressable {
    fn progress(&self, max: &Self) -> ProgressValue;
}

macro_rules! impl_progressable_int {
    ($ty: ty) => {
        impl Progressable for $ty {
            fn progress(&self, max: &Self) -> ProgressValue {
                ProgressValue {
                    numerator: *self as u64,
                    denominator: *max as u64,
                }
            }
        }
    };
    ($($ty: ty),+) => {
        $(impl_progressable_int!($ty);)+
    }
}

impl_progressable_int!(u8, u16, u32, u64, usize);
impl_progressable_int!(i8, i16, i32, i64, isize);

#[derive(Debug)]
pub enum BuildError {
    MissingLabel,
    MissingMax,
    MissingInit,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ProgressGroupBuilder {
    width: Option<usize>,
}

impl ProgressGroupBuilder {
    pub fn width(&mut self, width: usize) -> &mut Self {
        self.width = Some(width);
        self
    }

    pub fn build(&mut self) -> Arc<ProgressGroup> {
        let width = if let Some(width) = self.width {
            width
        } else {
            todo!("get width from terminal")
        };
        Arc::new(ProgressGroup::new(width))
    }
}

#[derive(Debug)]
pub struct ProgressGroup {
    width: usize,
    lines: AtomicUsize,
    label_width: AtomicUsize,
}

impl ProgressGroup {
    pub fn builder() -> ProgressGroupBuilder {
        ProgressGroupBuilder::default()
    }

    fn new(width: usize) -> Self {
        Self {
            width,
            lines: Default::default(),
            label_width: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct ProgressBuilder<L, T> {
    group: Arc<ProgressGroup>,
    label: Option<L>,
    max: Option<T>,
    init: Option<T>,
}

impl<L, T> ProgressBuilder<L, T> {
    fn new(group: Arc<ProgressGroup>) -> Self {
        Self {
            group,
            label: Default::default(),
            max: Default::default(),
            init: Default::default(),
        }
    }
}

impl<L, T> ProgressBuilder<L, T>
where
    L: Display,
    T: Ord + Display,
{
    pub fn label(&mut self, label: L) -> &mut Self {
        self.label = Some(label);
        self
    }

    pub fn max(&mut self, max: T) -> &mut Self {
        self.max = Some(max);
        self
    }

    pub fn init(&mut self, init: T) -> &mut Self {
        self.init = Some(init);
        self
    }

    pub fn build(&mut self) -> Result<Progress<L, T>, BuildError> {
        let Some(label) = self.label.take() else {
            return Err(BuildError::MissingLabel);
        };
        let Some(max) = self.max.take() else {
            return Err(BuildError::MissingMax);
        };
        let Some(init) = self.init.take() else {
            return Err(BuildError::MissingInit);
        };

        Ok(Progress::new(label, max, init, Arc::clone(&self.group)))
    }
}

pub struct Progress<L, T> {
    label: L,
    max: T,
    current: T,
    line: usize,
    group: Arc<ProgressGroup>,
}

impl<L, T> Progress<L, T> {
    pub fn builder(group: Arc<ProgressGroup>) -> ProgressBuilder<L, T> {
        ProgressBuilder::new(group)
    }

    fn new(label: L, max: T, current: T, group: Arc<ProgressGroup>) -> Self {
        // provide space to do things
        println!();
        Self {
            label,
            max,
            current,
            // TODO: ensure that this ordering is correct
            line: group
                .lines
                .fetch_add(1, std::sync::atomic::Ordering::Acquire)
                + 1,
            group,
        }
    }

    pub fn set_label(&mut self, label: L) -> L {
        std::mem::replace(&mut self.label, label)
    }
}

impl<L, T> Progress<L, T>
where
    L: Display,
    T: Ord + Display + Progressable,
{
    pub fn update(&mut self, new: T) {
        let progress = new.progress(&self.max);
        self.current = new;

        self.draw(progress);
    }

    fn draw(&self, progress: ProgressValue) {
        let line = self.group.lines.load(std::sync::atomic::Ordering::Relaxed) - self.line + 1;

        // TODO: determine how std deals with stderr errors and do the same

        let mut out = std::io::stderr().lock();

        // TODO: print directly to output (but we need its length)
        let label = self.label.to_string();

        self.group
            .label_width
            .fetch_max(label.len(), std::sync::atomic::Ordering::AcqRel);

        let label_width = self
            .group
            .label_width
            .load(std::sync::atomic::Ordering::Relaxed);

        // prefix:
        let _ = write!(
            out,
            concat!(
                "\x1b[?25l", // hide cursor
                "\x1b[{}A",  // move up `line` lines
                "\x1b[2N",   // clear line
                "\r",        // carriage return
            ),
            line
        );

        // width - label_width - len('[] ')
        let progress_width = self.group.width - label_width - 3;

        let progress = if progress.numerator >= progress.denominator {
            "=".repeat(progress_width)
        } else {
            let eqs = progress_width as u64 * progress.numerator / progress.denominator;
            std::iter::repeat_n('=', eqs as usize)
                .chain(std::iter::once('>'))
                .collect()
        };

        let _ = write!(
            out,
            "[{label:label_width$}] {progress:progress_width$} [{}/{}]",
            self.current, self.max,
        );

        // suffix:
        let _ = write!(
            out,
            concat!(
                "\x1b[{}B",  // move down `line` lines
                "\r",        // carriage return
                "\x1b[?25h", // show cursor
            ),
            line
        );

        let _ = out.flush();
    }
}
