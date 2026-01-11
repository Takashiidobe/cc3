use assert_cmd::Command;
use std::{fs, path::Path};

fn compile_to_asm(src: &str) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, src).expect("write input");
    let output_path = dir.path().join("out.s");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-S")
        .arg("-o")
        .arg(&output_path)
        .arg(&input_path)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    fs::read_to_string(&output_path).expect("read output")
}

fn has_label(asm: &str, name: &str) -> bool {
    let label = format!("{name}:");
    asm.lines().any(|line| line.trim() == label)
}

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
fn preprocess_skips_utf8_bom() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("bom.c");
    fs::write(&input_path, b"\xEF\xBB\xBFxyz\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd.arg("-E").arg(&input_path).output().expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = output.stdout;
    assert!(
        !stdout.starts_with(b"\xEF\xBB\xBF"),
        "stdout has BOM: {}",
        String::from_utf8_lossy(&stdout)
    );
    let stdout_text = String::from_utf8_lossy(&stdout);
    assert!(stdout_text.contains(" xyz"), "stdout: {stdout_text}");
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

#[test]
fn undef_option() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "foo\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-Dfoo=bar")
        .arg("-Ufoo")
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
    // After -U, foo should remain as the identifier "foo", not be replaced
    assert!(stdout.contains("foo"), "stdout: {stdout}");
    assert!(!stdout.contains("bar"), "stdout: {stdout}");
}

#[test]
fn ignored_options() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("empty.c");
    fs::write(&input_path, "int main() { return 0; }\n").expect("write input");
    let output_path = dir.path().join("out.o");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-c")
        .arg("-O")
        .arg("-Wall")
        .arg("-g")
        .arg("-std=c11")
        .arg("-ffreestanding")
        .arg("-fno-builtin")
        .arg("-fno-omit-frame-pointer")
        .arg("-fno-stack-protector")
        .arg("-fno-strict-aliasing")
        .arg("-m64")
        .arg("-mno-red-zone")
        .arg("-w")
        .arg("-o")
        .arg(&output_path)
        .arg(&input_path)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output_path.exists(), "output file should exist");
}

#[test]
fn static_inline_emission() {
    let asm = compile_to_asm("static inline void f1() {}");
    assert!(!has_label(&asm, "f1"), "asm: {asm}");

    let asm = compile_to_asm("static inline void f1() {} void foo() { f1(); }");
    assert!(has_label(&asm, "f1"), "asm: {asm}");

    let asm = compile_to_asm(
        "static inline void f1() {} static inline void f2() { f1(); } void foo() { f1(); }",
    );
    assert!(has_label(&asm, "f1"), "asm: {asm}");
    assert!(!has_label(&asm, "f2"), "asm: {asm}");

    let asm = compile_to_asm(
        "static inline void f1() {} static inline void f2() { f1(); } void foo() { f2(); }",
    );
    assert!(has_label(&asm, "f1"), "asm: {asm}");
    assert!(has_label(&asm, "f2"), "asm: {asm}");
}
