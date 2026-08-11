# Contributing to rtemis

Thank you for your interest in contributing to **rtemis**! This guide will help you report issues effectively.

## Before Opening an Issue

### Update to Latest Version

Ensure you're using the latest version of rtemis (v0.99+). Many issues may already be fixed in recent updates.

```r
# Install from CRAN
install.packages("rtemis")

# Install from GitHub
pak::pak("rtemis-org/rtemis")

# Install from r-universe
install.packages('rtemis', repos = 'https://rtemis-org.r-universe.dev')

# Check your version
packageVersion("rtemis")
```

### Check Existing Issues

Please search [existing issues](https://github.com/rtemis-org/rtemis/issues) to see if your problem or suggestion has already been reported. If you find a related issue, add a comment with any additional information.

### Review Documentation

- **API Documentation**: https://docs.rtemis.org/r/ml-api/
- **General Documentation**: https://docs.rtemis.org/r/ml

## Opening an Issue

### Issue Types

We welcome the following types of issues:

1. **🐛 Bug Reports**: Unexpected behavior, errors, or crashes. (Use `[BUG]` in the title)
2. **✨ Feature Requests**: Ideas for new functionality. (Use `[FEATURE]` in the title)
3. **📚 Documentation**: Improvements to docs or examples. (Use `[DOC]` in the title)
4. **❓ Questions**: Use [Discussions](https://github.com/rtemis-org/rtemis/discussions) for usage questions

### Bug Reports

A good bug report should include:

#### Required Information

1. **rtemis version**: Output of `utils::packageVersion("rtemis")`
2. **R version**: Output of `R.version.string`
3. **Operating System**: e.g., macOS 14.5, Ubuntu 22.04, Windows 11
4. **Clear description**: What did you expect vs. what actually happened?

#### Reproducible Example

**Critical**: Provide a minimal reproducible example. Use the template below:

```r
# Load required packages
library(rtemis)
library(data.table)  # if needed

# Create minimal data
set.seed(2025)
n <- 100
x <- rnormmat(n, 3)
y <- x[, 1] + x[, 2] + rnorm(n)
dat <- data.frame(x, y)

# Demonstrate the issue
mod <- train(
  x = dat,
  algorithm = "glm"
)

# Expected: Model trains successfully
# Actual: Error message...
```

#### Error Messages

Include **complete error messages** with full stack traces. If the error is verbose, use a code block:

```
Error in train(...):
! You must define either `hyperparameters` or `algorithm`.
```

#### Session Info (for complex issues)

For crashes or environment-specific issues, include:

```r
utils::sessionInfo()
```

### Feature Requests

For feature requests, please describe:

1. **Use case**: What problem would this solve?
2. **Proposed solution**: How should it work?
3. **Alternatives considered**: What workarounds exist currently?
4. **Impact**: Who would benefit from this feature?

**Example:**

> **Use case**: I frequently need to train models with time-series cross-validation but the current resampling methods don't preserve temporal order.
>
> **Proposed solution**: Add `setup_TimeSeriesCV()` that creates train/test splits respecting time ordering.
>
> **Alternatives**: Currently using custom resampling with `outer_resampling` parameter, but it's verbose and error-prone.

### Documentation Issues

For documentation improvements:

1. **Location**: Specify which page or function (e.g., `?train`, `?setup_GLMNET`)
2. **Problem**: What's unclear, incorrect, or missing?
3. **Suggestion**: How could it be improved?

## Version-Specific Notes

### rtemis 0.99+ vs. rtemisalpha (Legacy)

**Important**: This repository contains **rtemis 0.99+**, a complete rewrite using S7 classes. If you're using the legacy version (`rtemisalpha`), please note:

- Legacy issues should reference [rtemis-legacy](https://github.com/rtemis-org/rtemis-legacy) (unmaintained)
- Migration questions are welcome here
- API differences are expected (see README.md for major changes)

### Active Development

rtemis is under active development. Features may change between releases. When reporting issues:

- Specify your branch if not using `main` (check with `git branch`)
- Note if the issue appears in a specific algorithm (some are being ported from the legacy version)

## What Happens Next?

1. **Triage**: Maintainers will review and label your issue
2. **Discussion**: We may ask for clarification or additional details
3. **Resolution**: 
   - **Bugs**: Fixed in upcoming releases, referenced in commit messages
   - **Features**: Evaluated for inclusion in roadmap
   - **Questions**: Answered or redirected to appropriate resources

## Code of Conduct

Be respectful and constructive. We're all here to improve rtemis together.

## Pull Requests

Pull requests are welcome. Discuss major changes in an issue first, so that
design questions are settled before anyone writes code.

### Licensing of contributions

rtemis is released under the [BSD 3-Clause License](../LICENSE.md). By
submitting a pull request, patch, or any other contribution, you agree to all
of the following.

1. **Inbound equals outbound.** Your contribution is licensed under the BSD
   3-Clause License, the same terms that cover the rest of the package. You
   retain copyright in your own work.

2. **You grant the right to relicense.** You grant E.D. Gennatas a perpetual,
   worldwide, non-exclusive, royalty-free, irrevocable license to reproduce,
   modify, distribute and sublicense your contribution, including the right to
   distribute it under any OSI-approved license the project later adopts.

3. **You have the right to grant it.** Either you wrote the contribution
   yourself, or you have permission from its copyright holder to submit it
   under these terms. Do not submit code copied or adapted from a source under
   a copyleft license (GPL, AGPL, LGPL) or under any license whose terms
   conflict with BSD 3-Clause. If any part of your contribution originates
   elsewhere, say so in the pull request and name the source and its license.

Point 2 is the one that is easy to skip and expensive to add later. rtemis
changed license once already, from GPL (>= 3) to BSD 3-Clause in 1.3.5, and
doing so required establishing that every surviving line was the copyright
holder's own. A project that accepts contributions without an explicit grant
cannot make that kind of change again without tracking down every past
contributor for consent. The grant keeps the option open without asking anyone
to sign a separate agreement or assign their copyright.

### Sign your commits

Every commit must carry a `Signed-off-by` line certifying the
[Developer Certificate of Origin](https://developercertificate.org/):

```sh
git commit -s -m "Your commit message"
```

which appends a line matching your git `user.name` and `user.email`:

```
Signed-off-by: Your Name <you@example.com>
```

Amend an unsigned commit with `git commit --amend -s`, or a range with
`git rebase --signoff <base>`.

### Before you open a pull request

Development tasks go through the `justfile` rather than direct `Rscript` or
`R CMD` calls; `just --list` shows every recipe. Recipes chain, so `just
install` already runs `just document`, which already runs `just format`.

- `just install` -- format, document, and install
- `just test` -- run the test suite
- `just lint`, `just check-rd`, `just spell` -- add accepted terms to
  `inst/WORDLIST` by hand
- `just check-cran` -- run before claiming CRAN compliance

### Code conventions

**Classes and types.** The backend is S7 throughout. Build class properties
with the `prop_*` factories rather than declaring them by hand: one declaration
carries type, default, bounds, enum, tunability and description, and the S7
validator, the JSON Schema and the defaults artifact are all generated from it.
Hand-writing validation for a property usually means a factory argument was
missed.

Make a factory-built property optional with `nullable = TRUE`, not a union.
Declare a hand-written optional property as `NULL | <class>`, never
`<class> | NULL`: S7 takes a union's prototype from its **first** member, so
`class_integer | NULL` defaults to `integer(0)` rather than `NULL`, and every
`!is.null()` guard downstream silently misfires. `default = NULL` does not
help -- S7 reads it as "no default supplied" and falls back to the prototype.

`NULL` is the only unset value. Test for it with `is.null()`, not
`length(x) == 0L`.

**Validation.** Type-check and validate as early as possible, with corrective
error messages. Use the `check_*` helpers exported by rtemis.core
(`check_inherits`, `check_is_S7`, `check_pos_integer_scalar`, and friends).
Data-level checks specific to this package live in `R/check_data.R` and
`R/check_input_data.R`.

**Logging.** Use rtemis.core: `msg()`, `info()`, and `abort()` with the rtemis
error classes. Any function that can print to the console takes a `verbosity`
argument controlling how much.

**Style.**

- Type-stable code; never rely on implicit coercion
- Integer literals carry an `L` suffix: `n = 10L`
- Optional arguments default to `NULL`, with the real default set in the body
- Two blank lines between definitions
- US English: `behavior`, `normalize`, `analyze`, `license`
- ASCII only, everywhere. CRAN rejects non-ASCII characters; use hexadecimal
  Unicode escapes where a literal one is unavoidable
- Comments describe only the current state of the code. No history ("used to",
  "renamed from", "as of <date>") and no argument for why the project works the
  way it does. Git records what changed. Do document non-obvious mechanism that
  a future editor has to preserve

**Documentation.** roxygen2 on everything, with examples. Internal functions
get `@keywords internal` and `@noRd`. Document a `@param` as
`Class: Description ending with period.` Do not restate default values in the
description -- they already appear in the `Usage` section.

**Tests.** Include tests for new functionality.

## Questions?

- **General usage**: [GitHub Discussions](https://github.com/rtemis-org/rtemis/discussions)
- **Bug reports/features**: [GitHub Issues](https://github.com/rtemis-org/rtemis/issues)
- **Security issues**: Contact maintainers directly (see DESCRIPTION file)

---

Thank you for contributing to rtemis.
