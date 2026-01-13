// Assembly snapshot tests using cc3-as
// Follows the same pattern as tests/codegen.rs

use insta::assert_yaml_snapshot;
use serde::Serialize;
use std::{
    path::Path,
    process::{Command, Stdio},
};

#[derive(Serialize)]
struct TestOutput {
    stdout: String,
    stderr: String,
    status: i32,
}

fn run_case(path: &Path) -> datatest_stable::Result<()> {
    let output = Command::new(assert_cmd::cargo::cargo_bin!("cc3-as"))
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    let out = TestOutput {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status.code().unwrap_or(-1),
    };

    assert!(
        out.status == 0,
        "{} failed (status: {})\n--- stdout ---\n{}\n--- stderr ---\n{}",
        path.display(),
        out.status,
        out.stdout,
        out.stderr,
    );

    assert_yaml_snapshot!(path.to_str(), &out);

    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./asm-test/", pattern = r#"^.*.s$"# },
}
