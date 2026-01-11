use std::{
    env, fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

/// Get the path to the cc3 compiler binary
fn cc3_binary() -> PathBuf {
    assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")).to_path_buf()
}

/// Get the thirdparty directory path
fn thirdparty_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("thirdparty")
}

/// Clone a repository if it doesn't exist
fn clone_if_needed(repo_url: &str, target_dir: &Path) -> std::io::Result<()> {
    if target_dir.exists() {
        return Ok(());
    }

    let parent = target_dir.parent().unwrap();
    fs::create_dir_all(parent)?;

    let output = Command::new("git")
        .args(["clone", repo_url, target_dir.to_str().unwrap()])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    if !output.status.success() {
        panic!(
            "Failed to clone {}: {}",
            repo_url,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

/// Reset repository to a specific commit
fn git_reset(dir: &Path, commit: &str) -> std::io::Result<()> {
    let output = Command::new("git")
        .args(["reset", "--hard", commit])
        .current_dir(dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    if !output.status.success() {
        panic!(
            "Failed to reset to {}: {}",
            commit,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

/// Get number of CPUs for parallel make
fn num_jobs() -> String {
    Command::new("nproc")
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .and_then(|s| s.trim().parse::<usize>().ok())
        .map(|n| n.to_string())
        .unwrap_or_else(|| "4".to_string())
}

/// Run a command and ensure it succeeds
fn run_command(
    cmd: &str,
    args: &[&str],
    dir: &Path,
    env_vars: &[(&str, &str)],
) -> std::io::Result<()> {
    let mut command = Command::new(cmd);
    command
        .args(args)
        .current_dir(dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    for (key, value) in env_vars {
        command.env(key, value);
    }

    let output = command.output()?;

    if !output.status.success() {
        panic!(
            "Command '{}' failed with status: {:?}",
            cmd,
            output.status.code()
        );
    }

    Ok(())
}

#[test]
#[ignore] // Run with: cargo test --test thirdparty -- --ignored
fn test_tinycc() {
    let thirdparty = thirdparty_dir();
    let tinycc_dir = thirdparty.join("tinycc");
    let cc3 = cc3_binary();

    // Clone repository
    clone_if_needed("https://github.com/TinyCC/tinycc.git", &tinycc_dir).unwrap();

    // Reset to specific commit
    git_reset(&tinycc_dir, "df67d8617b7d1d03a480a28f9f901848ffbfb7ec").unwrap();

    // Configure
    run_command(
        "./configure",
        &[&format!("--cc={}", cc3.display())],
        &tinycc_dir,
        &[],
    )
    .unwrap();

    // Clean and build
    run_command("make", &["clean"], &tinycc_dir, &[]).unwrap();
    let jobs = num_jobs();
    run_command("make", &[&format!("-j{}", jobs)], &tinycc_dir, &[]).unwrap();

    // Run tests with system cc
    run_command("make", &["test"], &tinycc_dir, &[("CC", "cc")]).unwrap();
}

#[test]
#[ignore] // Run with: cargo test --test thirdparty -- --ignored
fn test_sqlite() {
    let thirdparty = thirdparty_dir();
    let sqlite_dir = thirdparty.join("sqlite");
    let cc3 = cc3_binary();

    // Clone repository
    clone_if_needed("https://github.com/sqlite/sqlite.git", &sqlite_dir).unwrap();

    // Reset to specific commit
    git_reset(&sqlite_dir, "86f477edaa17767b39c7bae5b67cac8580f7a8c1").unwrap();

    // Configure
    run_command(
        "./configure",
        &[],
        &sqlite_dir,
        &[("CC", cc3.to_str().unwrap()), ("CFLAGS", "-D_GNU_SOURCE")],
    )
    .unwrap();

    // Patch libtool
    let libtool_path = sqlite_dir.join("libtool");
    if libtool_path.exists() {
        run_command(
            "sed",
            &[
                "-i",
                "s/^wl=.*/wl=-Wl,/; s/^pic_flag=.*/pic_flag=-fPIC/",
                "libtool",
            ],
            &sqlite_dir,
            &[],
        )
        .unwrap();
    }

    // Clean and build
    run_command("make", &["clean"], &sqlite_dir, &[]).unwrap();
    let jobs = num_jobs();
    run_command("make", &[&format!("-j{}", jobs)], &sqlite_dir, &[]).unwrap();

    // Run tests
    run_command("make", &["test"], &sqlite_dir, &[]).unwrap();
}

#[test]
#[ignore] // Run with: cargo test --test thirdparty -- --ignored
fn test_libpng() {
    let thirdparty = thirdparty_dir();
    let libpng_dir = thirdparty.join("libpng");
    let cc3 = cc3_binary();

    // Clone repository
    clone_if_needed("https://github.com/rui314/libpng.git", &libpng_dir).unwrap();

    // Reset to specific commit
    git_reset(&libpng_dir, "dbe3e0c43e549a1602286144d94b0666549b18e6").unwrap();

    // Configure
    run_command(
        "./configure",
        &[],
        &libpng_dir,
        &[("CC", cc3.to_str().unwrap())],
    )
    .unwrap();

    // Patch libtool
    let libtool_path = libpng_dir.join("libtool");
    if libtool_path.exists() {
        run_command(
            "sed",
            &[
                "-i",
                "s/^wl=.*/wl=-Wl,/; s/^pic_flag=.*/pic_flag=-fPIC/",
                "libtool",
            ],
            &libpng_dir,
            &[],
        )
        .unwrap();
    }

    // Clean and build
    run_command("make", &["clean"], &libpng_dir, &[]).unwrap();
    let jobs = num_jobs();
    run_command("make", &[&format!("-j{}", jobs)], &libpng_dir, &[]).unwrap();

    // Run tests
    run_command("make", &["test"], &libpng_dir, &[]).unwrap();
}

#[test]
#[ignore] // Run with: cargo test --test thirdparty -- --ignored
fn test_git() {
    let thirdparty = thirdparty_dir();
    let git_dir = thirdparty.join("git");
    let cc3 = cc3_binary();

    // Clone repository
    clone_if_needed("https://github.com/git/git.git", &git_dir).unwrap();

    // Reset to specific commit
    git_reset(&git_dir, "54e85e7af1ac9e9a92888060d6811ae767fea1bc").unwrap();

    // Clean and build
    run_command("make", &["clean"], &git_dir, &[]).unwrap();
    let jobs = num_jobs();
    run_command(
        "make",
        &[
            &format!("-j{}", jobs),
            "V=1",
            &format!("CC={}", cc3.display()),
            "test",
        ],
        &git_dir,
        &[],
    )
    .unwrap();
}
