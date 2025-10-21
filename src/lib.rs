use std::{
    fmt::Display,
    io::Write,
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
        Arc, RwLock,
    },
    time::Instant,
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
    pub status_frame: (&'static str, &'static str),
    pub progress_frame: (&'static str, &'static str),
    /// None - don't show remaining time estimate
    pub remaining_frame: Option<(&'static str, &'static str)>,
    pub bar: char,
    pub end: &'static str,
    pub done: char,
    pub empty: char,
    pub use_percent: bool,
}

impl Default for ProgressStyle {
    fn default() -> Self {
        Self {
            label_frame: ("", " "),
            ratio_frame: (" ", ""),
            progress_frame: ("[", "]"),
            status_frame: ("  ", ""),
            remaining_frame: Some(("", " ")),
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
            terminal_size::terminal_size()
                .expect("failed to get terminal size")
                .0
                 .0 as _
        };
        Arc::new(ProgressGroup::new(width, self.style))
    }
}

#[derive(Debug)]
pub struct ProgressDisplayText {
    label: String,
    status: Option<String>,
    num_string: String,
    max_string: String,
}

#[derive(Debug)]
pub struct ProgressDisplay {
    text: RwLock<ProgressDisplayText>,
    numerator: AtomicU64,
    denominator: AtomicU64,
    started: Instant,
}

#[derive(Debug)]
pub struct ProgressGroup {
    items: RwLock<Vec<ProgressDisplay>>,
    width: usize,
    is_drawing: AtomicBool,
    style: ProgressStyle,
    /// [ 5/10]
    ///  ^^
    num_len: AtomicUsize,
    /// [ 5/10]
    ///     ^^
    den_len: AtomicUsize,
    label_len: AtomicUsize,
    status_len: AtomicUsize,
}

impl ProgressGroup {
    pub fn builder() -> ProgressGroupBuilder {
        ProgressGroupBuilder::default()
    }

    fn new(width: usize, style: ProgressStyle) -> Self {
        Self {
            items: RwLock::new(Vec::new()),
            width,
            is_drawing: AtomicBool::new(false),
            style,
            num_len: Default::default(),
            den_len: Default::default(),
            label_len: Default::default(),
            status_len: Default::default(),
        }
    }

    pub fn reset_widths(&self) {
        self.num_len.store(0, Ordering::Release);
        self.den_len.store(0, Ordering::Release);
        self.label_len.store(0, Ordering::Release);
        self.status_len.store(0, Ordering::Release);
    }

    pub fn draw(&self) {
        if self
            .is_drawing
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return;
        }

        let items = self.items.read().unwrap();

        let lens = items.iter().map(|x| {
            let text = x.text.read().unwrap();
            (
                text.status.as_ref().map(|x| x.len()),
                text.label.len(),
                text.num_string.len(),
                text.max_string.len(),
            )
        });
        let mut label_len = None;
        let mut status_len = None;
        let mut num_len = None;
        let mut den_len = None;

        for (status, label, num, den) in lens {
            if status_len.is_none_or(|s| s < status) {
                status_len = Some(status);
            }
            if label_len.is_none_or(|l| l < label) {
                label_len = Some(label);
            }
            if num_len.is_none_or(|s| s < num) {
                num_len = Some(num);
            }
            if den_len.is_none_or(|s| s < den) {
                den_len = Some(den);
            }
        }

        let (Some(label_len), Some(status_len), Some(num_len), Some(den_len)) =
            (label_len, status_len, num_len, den_len)
        else {
            return;
        };

        let label_len = self
            .label_len
            .fetch_max(label_len, Ordering::AcqRel)
            .max(label_len);
        let num_len = self
            .num_len
            .fetch_max(num_len, Ordering::AcqRel)
            .max(num_len);
        let den_len = self
            .den_len
            .fetch_max(den_len, Ordering::AcqRel)
            .max(den_len);
        let status_len = status_len.map(|status_len| {
            self.status_len
                .fetch_max(status_len, Ordering::AcqRel)
                .max(status_len)
        });

        let mut out = std::io::stderr().lock();

        // prefix
        let _ = write!(
            out,
            concat!(
                "\x1b[?25l", // hide cursor
                "\x1b[{}A",  // move up lines
                "\r",        // carriage return
            ),
            items.len(),
        );

        for progress in &*items {
            let text = progress.text.read().unwrap();
            let numerator = progress.numerator.load(Ordering::Relaxed);
            let denominator = progress.denominator.load(Ordering::Relaxed);

            // TODO: determine how std lib deals with stderr errors and do the same

            let status_len = status_len.unwrap_or(0)
                + self.style.status_frame.0.len()
                + self.style.status_frame.1.len();

            let num_str = if self.style.use_percent {
                None
            } else {
                Some(&text.num_string)
            };

            let progress_width = self.width
                - label_len
                - self.style.label_frame.0.len()
                - self.style.label_frame.1.len()
                - self.style.progress_frame.0.len()
                - self.style.progress_frame.1.len()
                - self.style.ratio_frame.0.len()
                - self.style.ratio_frame.1.len()
                - if let Some(frame) = self.style.remaining_frame {
                    // '00:00'
                    5 + frame.0.len() + frame.1.len()
                } else {
                    0
                }
                - if self.style.use_percent {
                    4 // '100%'
                } else {
                    num_len + 1 + den_len
                }
                - status_len;

            // TODO: make this not allocate
            let progress_text: String = if numerator >= denominator {
                std::iter::repeat_n(self.style.done, progress_width).collect()
            } else {
                let eqs = (progress_width as u64 * numerator / denominator) as usize;
                let end_len = self.style.end.chars().count();
                let len = eqs + if self.style.end.is_empty() { 0 } else { 1 };
                std::iter::repeat_n(
                    self.style.bar,
                    eqs.saturating_sub(end_len.saturating_sub(1)),
                )
                .chain(self.style.end.chars())
                .chain(std::iter::repeat_n(self.style.empty, progress_width - len))
                .collect()
            };

            // label
            let _ = write!(
                out,
                "{open}{:label_len$}{close}",
                text.label,
                open = self.style.label_frame.0,
                close = self.style.label_frame.1,
            );

            // remaining
            if let Some(ref frame) = self.style.remaining_frame {
                let elapsed = progress.started.elapsed();
                if numerator == 0 {
                    let _ = write!(out, "{open}--:--{close}", open = frame.0, close = frame.1,);
                } else {
                    let remaining = (elapsed.as_millis() as u64 * denominator / numerator)
                        .saturating_sub(elapsed.as_millis() as u64)
                        .div_ceil(1000);
                    let _ = write!(
                        out,
                        "{open}{:02}:{:02}{close}",
                        remaining / 60,
                        remaining % 60,
                        open = frame.0,
                        close = frame.1,
                    );
                }
            }

            // progress bar
            let _ = write!(
                out,
                "{open}{:progress_width$}{close}",
                progress_text,
                open = self.style.progress_frame.0,
                close = self.style.progress_frame.1,
            );

            // progress number
            let _ = if self.style.use_percent {
                write!(
                    out,
                    "{open}{:>3}%{close}",
                    (numerator * 100 / denominator).clamp(0, 100),
                    open = self.style.ratio_frame.0,
                    close = self.style.ratio_frame.1,
                )
            } else {
                write!(
                    out,
                    "{open}{:^num_len$}/{:^den_len$}{close}",
                    num_str.unwrap(),
                    text.max_string,
                    open = self.style.ratio_frame.0,
                    close = self.style.ratio_frame.1,
                )
            };

            if let Some(ref status) = text.status {
                let _ = write!(
                    out,
                    "{open}{}{close}",
                    status,
                    open = self.style.status_frame.0,
                    close = self.style.status_frame.1,
                );
            }

            // suffix
            let _ = write!(
                out,
                concat!(
                    "\x1b[0K", // clear rest of line
                    "\r",      // carriage return
                    "\x1b[1B", // down one line
                ),
            );
        }

        let _ = write!(
            out,
            "\x1b[?25h", // show cursor
        );
        let _ = out.flush();

        self.is_drawing.store(false, Ordering::Release);
    }
}

#[derive(Debug)]
pub struct ProgressBuilder<T, L> {
    group: Arc<ProgressGroup>,
    label: Option<L>,
    max: Option<T>,
    init: Option<T>,
}

impl<T, L> ProgressBuilder<T, L> {
    fn new(group: Arc<ProgressGroup>) -> Self {
        Self {
            group,
            label: Default::default(),
            max: Default::default(),
            init: Default::default(),
        }
    }
}

impl<T, L> ProgressBuilder<T, L>
where
    T: Display + Progressable,
    L: Display,
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
    index: usize,
    max: T,
    current: T,
    group: Arc<ProgressGroup>,
}

impl<T> Progress<T> {
    pub fn builder<L>(group: Arc<ProgressGroup>) -> ProgressBuilder<T, L> {
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

        let index = {
            let mut items = group.items.write().unwrap();
            let index = items.len();
            items.push(ProgressDisplay {
                text: RwLock::new(ProgressDisplayText {
                    label,
                    status: None,
                    max_string,
                    num_string: current.to_string(),
                }),
                numerator: AtomicU64::new(0),
                denominator: AtomicU64::new(1),
                started: Instant::now(),
            });
            index
        };

        let this = Self {
            index,
            max,
            current,
            group,
        };

        this.draw();

        this
    }

    pub fn set_label(&mut self, label: impl Display) {
        self.group.items.read().unwrap()[self.index]
            .text
            .write()
            .unwrap()
            .label = label.to_string();
        self.draw();
    }

    pub fn set_status(&mut self, status: impl Display) {
        self.group.items.read().unwrap()[self.index]
            .text
            .write()
            .unwrap()
            .status = Some(status.to_string());
        self.draw();
    }

    pub fn clear_status(&mut self) {
        self.group.items.read().unwrap()[self.index]
            .text
            .write()
            .unwrap()
            .status = None;
        self.draw();
    }

    pub fn update(&mut self, new: T) {
        self.current = new;

        let item = &self.group.items.read().unwrap()[self.index];

        item.text.write().unwrap().num_string = self.current.to_string();

        let prog = self.current.progress(&self.max);
        item.numerator.store(prog.numerator, Ordering::Release);
        item.denominator.store(prog.denominator, Ordering::Release);

        self.draw();
    }

    pub fn draw(&self) {
        self.group.draw();
    }
}
