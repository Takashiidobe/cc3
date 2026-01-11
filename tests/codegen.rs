use insta::assert_yaml_snapshot;
use insta_cmd::Command;
use serde::Serialize;
use std::{
    io,
    path::Path,
    process::{Command as StdCommand, Output, Stdio},
};

fn cc_command() -> StdCommand {
    if cfg!(target_arch = "x86_64") {
        StdCommand::new("clang")
    } else {
        let mut cmd = StdCommand::new("zig");
        cmd.arg("cc")
            .arg("--target=x86_64-linux-musl")
            .arg("-static");
        cmd
    }
}

fn compile(
    src: &Path,
    exe: &Path,
    extra_objs: &[&Path],
    extra_args: &[String],
) -> io::Result<Output> {
    let mut cmd = cc_command();
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
    let mut cmd = if cfg!(target_arch = "x86_64") {
        StdCommand::new(exe)
    } else {
        let qemu = std::env::var("CC3_QEMU_X86_64")
            .or_else(|_| std::env::var("QEMU_X86_64"))
            .unwrap_or_else(|_| "qemu-x86_64".to_string());
        let mut cmd = StdCommand::new(qemu);
        let prefix = std::env::var("CC3_QEMU_LD_PREFIX")
            .or_else(|_| std::env::var("QEMU_LD_PREFIX"))
            .ok()
            .filter(|value| !value.is_empty())
            .or_else(|| {
                let candidate = Path::new(env!("CARGO_MANIFEST_DIR")).join("qemu-rootfs");
                candidate
                    .is_dir()
                    .then_some(candidate.to_string_lossy().into_owned())
            });
        if let Some(prefix) = prefix {
            cmd.arg("-L").arg(prefix);
        }
        cmd.arg(exe);
        cmd
    };
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).output()
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
    let common_obj = tmp.path().join("common.o");

    let common_out = cc_command()
        .arg("-c")
        .arg("-o")
        .arg(&common_obj)
        .arg("-xc")
        .arg("test/common")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;
    ensure_success("cc(common)", Path::new("test/common"), &common_out);

    // 1) Your compiler: codegen -> .S, then cc -> exe_mine
    let bin = Command::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")));
    let mut codegen_cmd = bin;
    codegen_cmd.arg("-Iinclude").arg("-Itest");
    if path
        .file_name()
        .is_some_and(|name| name == "include-next.c")
    {
        codegen_cmd
            .arg("-Itest/include-next1")
            .arg("-Itest/include-next2")
            .arg("-Itest/include-next3");
    }
    let codegen_out = codegen_cmd
        .arg(path)
        .arg("-S")
        .arg("-o")
        .arg(&asm_path)
        .stderr(Stdio::piped())
        .output()?;
    ensure_success("codegen", path, &codegen_out);

    let compile_out_mine = compile(
        &asm_path,
        &exe_mine,
        &[common_obj.as_path()],
        &["-pthread".to_string()],
    )?;
    ensure_success("cc(asm)", path, &compile_out_mine);

    let run_out_mine = run_exe(&exe_mine)?;
    let mine = to_runlog(run_out_mine.clone());

    // Only persist snapshot if test passed (status 0)
    // This prevents accepting incorrect tests and shows what failed
    assert_eq!(
        mine.status,
        0,
        "[{}] test failed with status {}\n--- stdout ---\n{}",
        path.display(),
        mine.status,
        mine.stdout
    );

    assert_yaml_snapshot!(path.to_str(), &mine);

    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./test/", pattern = r#"^.*.c$"# },
}
