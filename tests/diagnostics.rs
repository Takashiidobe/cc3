use insta::assert_yaml_snapshot;
use serde::Serialize;
use std::process::{Command, Stdio};

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct DiagnosticLog {
    stderr: String,
    status: i32,
}

fn run_case(path: &std::path::Path) -> datatest_stable::Result<()> {
    let output = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")))
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    assert!(
        !output.status.success(),
        "[{}] expected failure, got success",
        path.display()
    );

    let log = DiagnosticLog {
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status.code().unwrap_or(-1),
    };

    assert_yaml_snapshot!(path.to_str(), &log);
    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./tests/invalid/", pattern = r#"^.*.c$"# },
}
