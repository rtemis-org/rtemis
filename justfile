# justfile
# ::rtemis::
# 2026- EDG rtemis.org

pkg := `awk '/^Package:/{print $2; exit}' DESCRIPTION`
r := env("R", "R")
rscript := env("RSCRIPT", "Rscript")
tarball_glob := pkg + "_*.tar.gz"

# Optional, and absent from a clone: the recipes that publish the generated
# schema registry need working copies of three other repositories, so they are
# kept with the notes that explain them. Everything below stands on its own.
import? '__dev/publish.just'

# List available recipes
default:
    @just --list

_msg msg:
    @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$(date '+%Y-%m-%d %H:%M:%S')" "{{ msg }}"

# Format R code with air CLI (if available)
format:
    @just _msg "─── Formatting {{ pkg }} package... ───"
    @if command -v air >/dev/null 2>&1; then \
        air format .; \
    else \
        echo "   Note: 'air' CLI not found -- skipping R code formatting."; \
    fi
    @just _msg "Done"

# Check R code formatting without modifying files (CI-friendly; fails if unformatted)
format-check:
    @just _msg "─── Checking formatting for {{ pkg }}... ───"
    @if command -v air >/dev/null 2>&1; then \
        air format --check .; \
    else \
        echo "   Error: 'air' CLI not found."; \
        exit 1; \
    fi
    @just _msg "Done"

# Generate roxygen2 documentation
document: format
    @just _msg "─── Documenting {{ pkg }} package... ───"
    {{ rscript }} -e "roxygen2::roxygenize()"
    @just _msg "Done"

# Lint package source for unused objects (variables/arguments) with lintr
lint:
    @just _msg "─── Linting {{ pkg }} source for unused objects... ───"
    {{ rscript }} -e "l <- lintr::lint_dir('R', linters = list(lintr::object_usage_linter())); print(l); if (length(l) > 0L) quit(status = 1L)"
    @just _msg "Done"

# Check that each man/*.Rd file has \value and \examples sections
check-rd:
    @just _msg "─── Checking Rd sections for {{ pkg }}... ───"
    tools/check-rd-sections.sh man
    @just _msg "Done"

# Like check-rd but also enforces \keyword{internal} docs (data/package stay exempt)
check-rd-all:
    @just _msg "─── Checking Rd sections (incl. internal) for {{ pkg }}... ───"
    tools/check-rd-sections.sh -internal man
    @just _msg "Done"

# Spell-check package; accepted technical terms live in inst/WORDLIST (see `spell-update`)
spell:
    @just _msg "─── Spell-checking {{ pkg }}... ───"
    {{ rscript }} -e "r <- spelling::spell_check_package(); print(r); if (nrow(r) > 0L) quit(status = 1L)"
    @just _msg "Done"

# Add all current spell-check terms to inst/WORDLIST (review the diff before committing)
spell-update:
    @just _msg "─── Updating inst/WORDLIST for {{ pkg }}... ───"
    {{ rscript }} -e "spelling::update_wordlist(confirm = FALSE)"
    @just _msg "Done"

# Document and install the package locally with pak
install: document
    @just _msg "─── Installing {{ pkg }} package... ───"
    {{ rscript }} -e "pak::local_install(upgrade = TRUE)"
    @just _msg "Done"

# Run testthat::test_local(stop_on_failure = TRUE)
test:
    @just _msg "─── Running testthat tests for {{ pkg }}... ───"
    {{ rscript }} -e "testthat::test_local(stop_on_failure = TRUE)"
    @just _msg "Done"

# `generate_checks.R` also refreshes the `inst/` copies of checks and traits,
# which are what the package ships and what `test_ChecksArtifact.R` reads, so a
# diff there after this runs means the committed copies were stale.
# `generate_checks_corpus.R` writes no `inst/` copy: nothing in R reads the
# corpus, it being generated *from* the tests that are its oracle.
[doc("Generate schemas + defaults + checks into a throwaway directory to assert the contracts")]
schemas-check:
    @just _msg "─── Checking schema generation for {{ pkg }}... ───"
    @dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT; \
        {{ rscript }} data-raw/generate_schemas.R "$dir" && \
        {{ rscript }} data-raw/generate_defaults.R "$dir" && \
        {{ rscript }} data-raw/generate_checks.R "$dir" && \
        {{ rscript }} data-raw/generate_checks_corpus.R "$dir" && \
        {{ rscript }} data-raw/generate_profile_fixture.R "$dir"
    @git diff --quiet --exit-code -- inst/checks inst/traits || { \
        echo "   Note: inst/checks or inst/traits was regenerated -- the committed copy was stale."; \
        echo "   Review the diff and commit it with the rule-set change."; \
    }
    @just _msg "Done"

# Build the source tarball
build: clean
    @just _msg "─── Building {{ pkg }} package... ───"
    {{ r }} CMD build .
    @just _msg "Done"

# Run R CMD check on the built tarball (pass extra flags, e.g. `just check --as-cran`)
check *flags: build
    @just _msg "─── Running R CMD check {{ flags }} on {{ pkg }}... ───"
    {{ r }} CMD check {{ tarball_glob }} {{ flags }}
    rm -f {{ tarball_glob }}
    @just _msg "Done"

# Run R CMD check --as-cran
check-cran: (check "--as-cran")

# Run R CMD check --as-cran --no-tests
check-cran-no-tests: (check "--as-cran" "--no-tests")

# Check URLs in package documentation with urlchecker
urls:
    @just _msg "─── Checking URLs for {{ pkg }}... ───"
    {{ rscript }} -e "urlchecker::url_check()"
    @just _msg "Done"

# Build package manual (PDF)
manual:
    @just _msg "─── Building manual for {{ pkg }}... ───"
    {{ r }} CMD Rd2pdf . --output={{ pkg }}.pdf
    @just _msg "Done"

# Build pkgdown site
site:
    @just _msg "─── Building pkgdown site for {{ pkg }}... ───"
    {{ rscript }} -e "pkgdown::build_site()"
    @just _msg "Done"

# Run rhub checks across CRAN platforms
rhub-check:
    @just _msg "─── Running rhub checks for {{ pkg }}... ───"
    {{ rscript }} -e "rhub::rhub_check(platforms = c('linux', 'macos-arm64', 'windows'))"
    @just _msg "Done"

# Run reverse dependency checks
revdep:
    @just _msg "─── Running reverse dependency checks for {{ pkg }}... ───"
    {{ rscript }} -e "revdepcheck::revdep_reset()"
    {{ rscript }} -e "revdepcheck::revdep_check(num_workers = 6)"
    @just _msg "Done"

# Remove tarballs and .Rcheck output
clean:
    @just _msg "─── Cleaning build artifacts... ───"
    rm -rf {{ pkg }}.Rcheck
    rm -f {{ tarball_glob }}
    @just _msg "Done"
