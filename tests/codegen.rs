use insta::assert_yaml_snapshot;
use insta_cmd::Command;
use serde::Serialize;
use std::{
    io,
    path::Path,
    process::{Command as StdCommand, Output, Stdio},
};

fn compile(
    src: &Path,
    exe: &Path,
    extra_objs: &[&Path],
    extra_args: &[String],
) -> io::Result<Output> {
    let mut cmd = StdCommand::new("clang");
    cmd.arg("-o").arg(exe).arg(src);
    for arg in extra_args {
        cmd.arg(arg);
    }
    for obj in extra_objs {
        cmd.arg(obj);
    }
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).output()
}

fn run_exe(exe: &Path) -> io::Result<Output> {
    StdCommand::new(exe)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct RunLog {
    stdout: String,
    status: i32,
}

fn to_runlog(out: Output) -> RunLog {
    RunLog {
        stdout: String::from_utf8_lossy(&out.stdout).to_string(),
        status: out.status.code().unwrap_or(-1),
    }
}

fn ensure_success(tag: &str, path: &Path, out: &Output) {
    assert!(
        out.status.success(),
        "[{}] {} failed (status: {:?})\n--- stdout ---\n{}\n--- stderr ---\n{}",
        path.display(),
        tag,
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}

fn run_case(path: &Path) -> datatest_stable::Result<()> {
    let tmp = tempfile::tempdir()?;
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    let asm_path = tmp.path().join(format!("{stem}.S"));
    let exe_mine = tmp.path().join(format!("{stem}.mine"));
    let helper_src = tmp.path().join("helper.c");
    let helper_obj = tmp.path().join("helper.o");

    std::fs::write(
        &helper_src,
        concat!(
            "int ret3() { return 3; }\n",
            "int ret5() { return 5; }\n",
            "int add(int x, int y) { return x + y; }\n",
            "int sub(int x, int y) { return x - y; }\n",
            "int add6(int a, int b, int c, int d, int e, int f) {\n",
            "  return a + b + c + d + e + f;\n",
            "}\n",
        ),
    )?;
    let helper_out = StdCommand::new("clang")
        .arg("-c")
        .arg("-o")
        .arg(&helper_obj)
        .arg(&helper_src)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;
    ensure_success("cc(helper)", &helper_src, &helper_out);

    // 1) Your compiler: codegen -> .S, then cc -> exe_mine
    let mut bin = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let codegen_out = bin
        .arg(path)
        .arg("-o")
        .arg(&asm_path)
        .stderr(Stdio::piped())
        .output()?;
    eprintln!(
        "[{}] codegen status: {:?}",
        path.display(),
        codegen_out.status.code()
    );
    ensure_success("codegen", path, &codegen_out);

    let compile_out_mine = compile(&asm_path, &exe_mine, &[helper_obj.as_path()], &[])?;
    eprintln!(
        "[{}] cc(asm) status: {:?}",
        path.display(),
        compile_out_mine.status.code()
    );
    ensure_success("cc(asm)", path, &compile_out_mine);

    let run_out_mine = run_exe(&exe_mine)?;
    let mine = to_runlog(run_out_mine);

    assert_yaml_snapshot!(path.to_str(), &mine);

    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./tests/files/", pattern = r#"^.*.c$"# },
}
