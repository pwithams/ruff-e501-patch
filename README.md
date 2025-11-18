# Patch to add E501 autofixing to Ruff

Main file changed is `crates/ruff_linter/src/rules/pycodestyle/rules/line_too_long.rs`

See https://github.com/astral-sh/ruff

Background for why feature is not included in Ruff itself: https://github.com/astral-sh/ruff/issues/19346

## Usage

```
ruff_e501_patch check [path or file] --preview --select E501,W505 --unsafe-fixes --fix
```
