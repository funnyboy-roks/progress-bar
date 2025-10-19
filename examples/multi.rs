use std::{sync::Arc, thread::JoinHandle};

use prog::{Progress, ProgressGroup};

fn main() {
    let mut threads = Vec::new();
    let group = ProgressGroup::builder().width(100).build();

    macro_rules! bar {
        ($label: expr, ($start: expr, $end: expr), $time_ms: expr, $delayed_ms: expr) => {
            let group = Arc::clone(&group);
            threads.push(std::thread::spawn(move || {
                let mut current = $start;
                std::thread::sleep(std::time::Duration::from_millis($delayed_ms));
                let mut prog = Progress::builder(group)
                    .label($label)
                    .max($end)
                    .init($start)
                    .build()
                    .unwrap();
                while current <= $end {
                    prog.update(current);
                    std::thread::sleep(std::time::Duration::from_millis(
                        $time_ms / ($end - $start),
                    ));
                    current += 1;
                }
            }));
        };
    }

    bar!("My progress bar", (0, 1000), 1000, 0);
    bar!("My second progress bar", (0, 2000), 5000, 0);
    bar!("My third progress bar", (0, 10), 2000, 1000);
    bar!(
        "My very slow progress bar with a long name",
        (0, 10),
        5000,
        500
    );

    threads.into_iter().try_for_each(JoinHandle::join).unwrap();

    drop(group);
}
