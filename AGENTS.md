# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Context

This project is called `cc3`. It is a C compiler written in rust,
following the commits of `chibicc`. Each commit message and diff in
`chibicc` is located in the `diffs` dir. Each beads task contains the
diff of the task to work on, e.g.

```sh
$ bd show f7x

cc3-f7x: 0. Compile an integer to an exectuable that exits with the given number
Status: open
Priority: P2
Type: task
Created: 2025-12-31 15:08
Created by: takashi
Updated: 2025-12-31 15:08

Description:
Diff: Refer to diffs/0.diff

Blocks (1):
  ← cc3-cm5: 1. Add + and - operators [P2 - open]
```

You can look at the diff in order to see what needs to be implemented in
the task.

**ALWAYS** use rust edition 2024.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **ALWAYS add integration tests instead of manually running compiled binaries.**
   - **Create test files in `tests/files/`**
   - Add `.c` test files that assert that new functionality works as
     expected
   - **Run the test suite**
   ```bash
   cargo test
   ```
   - **If the test harness fails, fix them**
   - **Accept new snapshots**
   ```bash
   cargo insta accept
   ```
   - **Verify all tests pass**
   ```bash
   cargo test
   ```
3. Run `cargo clippy --all-targets` and fix any linter errors
4. Run `cargo fmt` to format code
5. **Update issue status** - Close finished work, update in-progress items
6. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
7. **Clean up** - Clear stashes, prune remote branches
8. **Verify** - All changes committed AND pushed
9. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
