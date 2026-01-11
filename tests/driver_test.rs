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
fn preprocess_dash_m_outputs_dependencies() {
    let dir = tempfile::tempdir().expect("tempdir");
    let header1 = dir.path().join("out2.h");
    let header2 = dir.path().join("out3.h");
    fs::write(&header1, "foo\n").expect("write header1");
    fs::write(&header2, "bar\n").expect("write header2");

    let input_path = dir.path().join("input.c");
    let input = "#include \"out2.h\"\n#include \"out3.h\"\n";
    fs::write(&input_path, input).expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-M")
        .arg(format!("-I{}", dir.path().display()))
        .arg(&input_path)
        .output()
        .expect("run cc3 -M");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.starts_with("input.o:"), "stdout: {stdout}");
    assert!(
        stdout.contains(&input_path.display().to_string()),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains(&header1.display().to_string()),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains(&header2.display().to_string()),
        "stdout: {stdout}"
    );
}

#[test]
fn preprocess_dash_mf_writes_dependencies_to_file() {
    let dir = tempfile::tempdir().expect("tempdir");
    let header1 = dir.path().join("out2.h");
    let header2 = dir.path().join("out3.h");
    fs::write(&header1, "foo\n").expect("write header1");
    fs::write(&header2, "bar\n").expect("write header2");

    let input_path = dir.path().join("input.c");
    let input = "#include \"out2.h\"\n#include \"out3.h\"\n";
    fs::write(&input_path, input).expect("write input");

    let dep_path = dir.path().join("deps.d");
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-M")
        .arg("--MF")
        .arg(&dep_path)
        .arg(format!("-I{}", dir.path().display()))
        .arg(&input_path)
        .output()
        .expect("run cc3 -MF");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let deps = fs::read_to_string(&dep_path).expect("read deps");
    assert!(deps.starts_with("input.o:"), "deps: {deps}");
    assert!(
        deps.contains(&input_path.display().to_string()),
        "deps: {deps}"
    );
    assert!(
        deps.contains(&header1.display().to_string()),
        "deps: {deps}"
    );
    assert!(
        deps.contains(&header2.display().to_string()),
        "deps: {deps}"
    );
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

#[test]
fn idirafter_option() {
    let dir = tempfile::tempdir().expect("tempdir");
    let dir1 = dir.path().join("dir1");
    let dir2 = dir.path().join("dir2");
    fs::create_dir(&dir1).expect("create dir1");
    fs::create_dir(&dir2).expect("create dir2");

    let header1 = dir1.join("idirafter.h");
    let header2 = dir2.join("idirafter.h");
    fs::write(&header1, "foo\n").expect("write header1");
    fs::write(&header2, "bar\n").expect("write header2");

    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "#include \"idirafter.h\"\n").expect("write input");

    // Test 1: With both -I, first one should be found
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg(format!("-I{}", dir1.display()))
        .arg(format!("-I{}", dir2.display()))
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
    assert!(stdout.contains("foo"), "stdout: {stdout}");

    // Test 2: With -idirafter for dir1 and -I for dir2, dir2 should be found first
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg(format!("--idirafter={}", dir1.display()))
        .arg(format!("-I{}", dir2.display()))
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
fn fcommon_flag() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "int foo;\n").expect("write input");
    let output_path = dir.path().join("out.s");

    // Test default behavior (should emit .comm)
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
    let asm = fs::read_to_string(&output_path).expect("read output");
    assert!(asm.contains(".comm foo"), "asm: {asm}");

    // Test -fcommon (should emit .comm)
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-fcommon")
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
    let asm = fs::read_to_string(&output_path).expect("read output");
    assert!(asm.contains(".comm foo"), "asm: {asm}");
}

#[test]
fn fno_common_flag() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "int foo;\n").expect("write input");
    let output_path = dir.path().join("out.s");

    // Test -fno-common (should emit foo: label)
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-fno-common")
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
    let asm = fs::read_to_string(&output_path).expect("read output");
    assert!(asm.contains("foo:"), "asm: {asm}");
    assert!(!asm.contains(".comm foo"), "asm: {asm}");
}

#[test]
fn include_option() {
    let dir = tempfile::tempdir().expect("tempdir");
    let header_path = dir.path().join("out.h");
    fs::write(&header_path, "foo\n").expect("write header");

    let input_path = dir.path().join("input.c");
    fs::write(&input_path, "bar\n").expect("write input");

    // Test 1: -include should include the file before main input
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg(format!("--include={}", header_path.display()))
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
    assert!(stdout.contains("foo"), "stdout: {stdout}");
    assert!(stdout.contains("bar"), "stdout: {stdout}");
    // foo should appear before bar
    let foo_pos = stdout.find("foo").expect("find foo");
    let bar_pos = stdout.find("bar").expect("find bar");
    assert!(foo_pos < bar_pos, "foo should appear before bar");

    // Test 2: -include with standard header
    let input_path2 = dir.path().join("input2.c");
    fs::write(&input_path2, "NULL\n").expect("write input2");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-Iinclude")
        .arg("--include=stdio.h")
        .arg("-E")
        .arg(&input_path2)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("0"), "stdout: {stdout}");
}

#[test]
fn x_option() {
    let dir = tempfile::tempdir().expect("tempdir");
    let output_path = dir.path().join("foo.o");

    // Test 1: -xc for C input from a file without .c extension
    let input_path = dir.path().join("input.txt");
    fs::write(&input_path, "int x;\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-c")
        .arg("-xc")
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

    // Test 2: -x assembler for assembly input
    let asm_input = dir.path().join("input.txt");
    fs::write(&asm_input, "x:\n").expect("write asm");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-c")
        .arg("-x")
        .arg("assembler")
        .arg("-o")
        .arg(&output_path)
        .arg(&asm_input)
        .output()
        .expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output_path.exists(), "output file should exist");

    // Test 3: -x none resets to extension-based detection
    let c_input = dir.path().join("input.c");
    fs::write(&c_input, "int x;\n").expect("write c");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd
        .arg("-c")
        .arg("-x")
        .arg("assembler")
        .arg("-x")
        .arg("none")
        .arg("-o")
        .arg(&output_path)
        .arg(&c_input)
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
fn preprocess_only_implies_c_language() {
    // Test that -E implies -xc, allowing preprocessing from stdin without explicit -xc
    let dir = tempfile::tempdir().expect("tempdir");
    let input_path = dir.path().join("input.txt");
    fs::write(&input_path, "foo\n").expect("write input");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let output = cmd.arg("-E").arg(&input_path).output().expect("run cc3");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("foo"), "stdout: {stdout}");
}
