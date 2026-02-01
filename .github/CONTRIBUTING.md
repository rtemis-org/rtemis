# Contributing to rtemis

Thank you for your interest in contributing to **rtemis**! This guide will help you report issues effectively.

## Before Opening an Issue

### Update to Latest Version

Ensure you're using the latest version of rtemis (v0.99+). Many issues may already be fixed in recent updates.

```r
# Update from GitHub
pak::pkg_install("rtemis-org/rtemis")

# Or from r-universe
install.packages('rtemis', repos = 'https://egenn.r-universe.dev')

# Check your version
packageVersion("rtemis")
```

### Check Existing Issues

Please search [existing issues](https://github.com/rtemis-org/rtemis/issues) to see if your problem or suggestion has already been reported. If you find a related issue, add a comment with any additional information.

### Review Documentation

- **API Documentation**: https://rdocs.rtemis.org/api/
- **General Documentation**: https://rdocs.rtemis.org
- **Copilot Instructions**: `.github/copilot-instructions.md` (for developers)

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

1. **rtemis version**: Output of `packageVersion("rtemis")`
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
sessionInfo()
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

rtemis 0.99+ is under active development. Features may change between releases. When reporting issues:

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

While this guide focuses on issues, pull requests are welcome! Key points:

- Discuss major changes in an issue first
- Follow existing code style (S7 classes, roxygen2 documentation)
- All `@param` must follow format: `Class: Description ending with period.`
- Include tests for new functionality
- Update documentation as needed

See `.github/copilot-instructions.md` for detailed coding conventions.

## Questions?

- **General usage**: [GitHub Discussions](https://github.com/rtemis-org/rtemis/discussions)
- **Bug reports/features**: [GitHub Issues](https://github.com/rtemis-org/rtemis/issues)
- **Security issues**: Contact maintainers directly (see DESCRIPTION file)

---

Thank you for contributing to rtemis.
