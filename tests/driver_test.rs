use assert_cmd::Command;
use std::{fs, path::Path};

#[test]
fn preprocess_dash_e_outputs_includes() {
    let dir = tempfile::tempdir().expect("tempdir");
    let include_path = dir.path().join("inc.h");
    fs::write(&include_path, "foo\n").expect("write include");

    let input_path = dir.path().join("input.c");
    let input = format!("#include \"{}\"\n", include_path.display());
    fs::write(&input_path, input).expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd.arg("-E").arg(&input_path).output().expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("foo"), "stdout: {stdout}");

    let out_path = dir.path().join("out.i");
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-E")
        .arg("-o")
        .arg(&out_path)
        .arg(&input_path)
        .output()
        .expect("run cc3 -o");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let out_contents = fs::read_to_string(&out_path).expect("read output");
    assert!(out_contents.contains("foo"), "output: {out_contents}");
}

#[test]
fn default_include_paths() {
    let dir = tempfile::tempdir().expect("tempdir");
    let bin = assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME"));
    let bin_path = Path::new(&bin);
    let include_dir = bin_path.parent().expect("bin dir").join("include");
    fs::create_dir_all(&include_dir).expect("create include dir");

    let header_name = format!("cc3-default-include-{}.h", std::process::id());
    let header_path = include_dir.join(&header_name);
    fs::write(&header_path, "bar\n").expect("write header");

    let input_path = dir.path().join("input.c");
    let input = format!("#include <{header_name}>\n");
    fs::write(&input_path, input).expect("write input");

    let mut cmd = Command::new(bin);
    let output = cmd.arg("-E").arg(&input_path).output().expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("bar"), "stdout: {stdout}");
}

#[test]
fn define_option_without_value() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "foo\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-Dfoo")
        .arg("-E")
        .arg(&input_path)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("1"), "stdout: {stdout}");
}

#[test]
fn define_option_with_value() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "foo\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-Dfoo=bar")
        .arg("-E")
        .arg(&input_path)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("bar"), "stdout: {stdout}");
}
