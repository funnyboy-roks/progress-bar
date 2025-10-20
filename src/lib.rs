use std::{
    fmt::Display,
    io::Write,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

// TODO: ensure that atomic ordering is correct

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

#[derive(Debug, Clone, Copy)]
pub struct ProgressStyle {
    pub label_frame: (&'static str, &'static str),
    pub ratio_frame: (&'static str, &'static str),
    pub bar: char,
    pub end: &'static str,
    pub done: char,
    pub empty: char,
    pub use_percent: bool,
}

impl Default for ProgressStyle {
    fn default() -> Self {
        Self {
            label_frame: ("", "     ["),
            ratio_frame: ("] ", ""),
            bar: '=',
            end: ">",
            done: '=',
            empty: ' ',
            use_percent: false,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ProgressGroupBuilder {
    width: Option<usize>,
    style: ProgressStyle,
}

impl ProgressGroupBuilder {
    pub fn width(&mut self, width: usize) -> &mut Self {
        self.width = Some(width);
        self
    }

    pub fn style(&mut self, style: ProgressStyle) -> &mut Self {
        self.style = style;
        self
    }

    pub fn build(&mut self) -> Arc<ProgressGroup> {
        let width = if let Some(width) = self.width {
            width
        } else {
            todo!("get width from terminal")
        };
        Arc::new(ProgressGroup::new(width, self.style))
    }
}

#[derive(Debug)]
pub struct ProgressGroup {
    width: usize,
    style: ProgressStyle,
    lines: AtomicUsize,
    /// [ 5/10]
    ///  ^^
    num_len: AtomicUsize,
    /// [ 5/10]
    ///     ^^
    den_len: AtomicUsize,
    label_width: AtomicUsize,
}

impl ProgressGroup {
    pub fn builder() -> ProgressGroupBuilder {
        ProgressGroupBuilder::default()
    }

    fn new(width: usize, style: ProgressStyle) -> Self {
        Self {
            width,
            style,
            lines: Default::default(),
            num_len: Default::default(),
            den_len: Default::default(),
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
    T: Display + Progressable,
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

    pub fn build(&mut self) -> Result<Progress<T>, BuildError> {
        let Some(label) = self.label.take() else {
            return Err(BuildError::MissingLabel);
        };
        let Some(max) = self.max.take() else {
            return Err(BuildError::MissingMax);
        };
        let Some(init) = self.init.take() else {
            return Err(BuildError::MissingInit);
        };

        Ok(Progress::new(
            label.to_string(),
            max,
            init,
            Arc::clone(&self.group),
        ))
    }
}

pub struct Progress<T> {
    label: String,
    max: T,
    max_string: String,
    current: T,
    line: usize,
    group: Arc<ProgressGroup>,
}

impl<T> Progress<T> {
    pub fn builder<L>(group: Arc<ProgressGroup>) -> ProgressBuilder<L, T> {
        ProgressBuilder::new(group)
    }
}

impl<T> Progress<T>
where
    T: Display + Progressable,
{
    fn new(label: String, max: T, current: T, group: Arc<ProgressGroup>) -> Self {
        // create the line that will be used by the progress bar
        println!();

        let max_string = max.to_string();

        group.den_len.fetch_max(max_string.len(), Ordering::AcqRel);

        let this = Self {
            label,
            max,
            max_string,
            current,
            line: group.lines.fetch_add(1, Ordering::Acquire) + 1,
            group,
        };

        this.draw(ProgressValue {
            numerator: 0,
            denominator: 1,
        });

        this
    }

    pub fn set_label(&mut self, label: impl Display) {
        self.label = label.to_string();
        self.draw(self.current.progress(&self.max));
    }

    pub fn update(&mut self, new: T) {
        let progress = new.progress(&self.max);
        self.current = new;

        self.draw(progress);
    }

    fn draw(&self, p: ProgressValue) {
        let line = self.group.lines.load(Ordering::Relaxed) - self.line + 1;

        // TODO: determine how std lib deals with stderr errors and do the same

        let mut out = std::io::stderr().lock();

        self.group
            .label_width
            .fetch_max(self.label.len(), Ordering::AcqRel);

        // prefix
        let _ = write!(
            out,
            concat!(
                "\x1b[?25l",    // hide cursor
                "\x1b[{line}A", // move up `line` lines
                "\r",           // carriage return
            ),
            line = line, // write! + concat! is funky
        );

        let label_width = self.group.label_width.load(Ordering::Relaxed);
        let (num_str, num_len, den_len) = if self.group.style.use_percent {
            (None, 0, 0)
        } else {
            let num_str = self.current.to_string();
            let num_str_len = num_str.len();
            (
                Some(num_str),
                self.group
                    .num_len
                    .fetch_max(num_str_len, Ordering::AcqRel)
                    .max(num_str_len),
                self.group.den_len.load(Ordering::Relaxed),
            )
        };

        // width - label_width - len('[] ')
        let progress_width = self.group.width
            - label_width
            - self.group.style.label_frame.0.len()
            - self.group.style.label_frame.1.len()
            - self.group.style.ratio_frame.0.len()
            - self.group.style.ratio_frame.1.len()
            - if self.group.style.use_percent {
                4 // len('100%')
            } else {
                num_len + 1 + den_len
            };

        // TODO: make this not allocate
        let progress: String = if p.numerator >= p.denominator {
            std::iter::repeat_n(self.group.style.done, progress_width).collect()
        } else {
            let eqs = (progress_width as u64 * p.numerator / p.denominator) as usize;
            let end_len = self.group.style.end.chars().count();
            let len = eqs
                + if self.group.style.end.is_empty() {
                    0
                } else {
                    1
                };
            std::iter::repeat_n(
                self.group.style.bar,
                eqs.saturating_sub(end_len.saturating_sub(1)),
            )
            .chain(self.group.style.end.chars())
            .chain(std::iter::repeat_n(
                self.group.style.empty,
                progress_width - len,
            ))
            .collect()
        };

        // label
        let _ = write!(
            out,
            "{open}{:label_width$}{close}",
            self.label,
            open = self.group.style.label_frame.0,
            close = self.group.style.label_frame.1,
        );

        // progress bar
        let _ = write!(out, "{:progress_width$}", progress);

        // progress number
        let _ = if self.group.style.use_percent {
            write!(
                out,
                "{open}{:>3}%{close}",
                (p.numerator * 100 / p.denominator).clamp(0, 100),
                open = self.group.style.ratio_frame.0,
                close = self.group.style.ratio_frame.1,
            )
        } else {
            write!(
                out,
                "{open}{:^num_len$}/{:^den_len$}{close}",
                num_str.unwrap(),
                self.max_string,
                open = self.group.style.ratio_frame.0,
                close = self.group.style.ratio_frame.1,
            )
        };

        // suffix
        let _ = write!(
            out,
            concat!(
                "\x1b[0K",      // clear rest of line
                "\x1b[{line}B", // move down `line` lines
                "\r",           // carriage return
                "\x1b[?25h",    // show cursor
            ),
            line = line, // write! + concat! is funky
        );

        let _ = out.flush();
    }
}
