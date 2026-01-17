use insta::assert_yaml_snapshot;
use serde::Serialize;
use std::{
    path::Path,
    process::{Command, Stdio},
};

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct WarningLog {
    stderr: String,
    status: i32,
}

fn run_case(path: &Path) -> datatest_stable::Result<()> {
    let tmp = tempfile::tempdir()?;
    let asm_path = tmp.path().join("out.S");

    let output = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")))
        .arg("-Iinclude")
        .arg("-Itest")
        .arg(path)
        .arg("-S")
        .arg("-o")
        .arg(&asm_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    assert!(
        output.status.success(),
        "[{}] expected success, got failure",
        path.display()
    );

    let log = WarningLog {
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status.code().unwrap_or(-1),
    };

    assert_yaml_snapshot!(path.to_str(), &log);
    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./tests/warnings/", pattern = r#"^.*.c$"# },
}
