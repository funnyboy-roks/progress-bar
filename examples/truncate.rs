use std::time::Duration;

use prog::{Progress, ProgressGroup, ProgressStyle};

fn main() {
    let group = ProgressGroup::builder()
        .width(108)
        .progress_width(80)
        .style(ProgressStyle::builder().use_percent(true))
        .build();

    let thread = {
        let mut prog = Progress::builder(group.clone())
            .label("hello")
            .init(0)
            .max(100)
            .build()
            .unwrap();
        std::thread::spawn({
            move || {
                for i in 1..=100 {
                    prog.update(i);
                    prog.set_status("hello world");
                    std::thread::sleep(Duration::from_millis(20));
                }
                prog.set_status("\x1b[32mDone!\x1b[0m");
            }
        })
    };

    thread.join().unwrap();
    drop(group);
    println!("Hello world");
}
