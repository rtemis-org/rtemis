# justfile
# ::rtemis::
# 2026- EDG rtemis.org

pkg := `awk '/^Package:/{print $2; exit}' DESCRIPTION`
r := env("R", "R")
rscript := env("RSCRIPT", "Rscript")
tarball_glob := pkg + "_*.tar.gz"

# List available recipes
default:
    @just --list

_msg msg:
    @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$(date '+%Y-%m-%d %H:%M:%S')" "{{msg}}"

# Format R code with air CLI (if available)
format:
    @just _msg "─── Formatting {{pkg}} package... ───"
    @if command -v air >/dev/null 2>&1; then \
        air format .; \
    else \
        echo "   Note: 'air' CLI not found — skipping R code formatting."; \
    fi
    @just _msg "Done"

# Generate roxygen2 documentation
document: format
    @just _msg "─── Documenting {{pkg}} package... ───"
    {{rscript}} -e "roxygen2::roxygenize()"
    @just _msg "Done"

# Document and install the package locally with pak
install: document
    @just _msg "─── Installing {{pkg}} package... ───"
    {{rscript}} -e "pak::local_install(upgrade = TRUE)"
    @just _msg "Done"

# Run testthat::test_local(stop_on_failure = TRUE)
test:
    @just _msg "─── Running testthat tests for {{pkg}}... ───"
    {{rscript}} -e "testthat::test_local(stop_on_failure = TRUE)"
    @just _msg "Done"

# Build the source tarball
build: clean
    @just _msg "─── Building {{pkg}} package... ───"
    {{r}} CMD build .
    @just _msg "Done"

# Run R CMD check on the built tarball (pass extra flags, e.g. `just check --as-cran`)
check *flags: build
    @just _msg "─── Running R CMD check {{flags}} on {{pkg}}... ───"
    {{r}} CMD check {{tarball_glob}} {{flags}}
    rm -f {{tarball_glob}}
    @just _msg "Done"

# Run R CMD check --as-cran
check-cran: (check "--as-cran")

# Run R CMD check --as-cran --no-tests
check-cran-no-tests: (check "--as-cran" "--no-tests")

# Check URLs in package documentation with urlchecker
urls:
    @just _msg "─── Checking URLs for {{pkg}}... ───"
    {{rscript}} -e "urlchecker::url_check()"
    @just _msg "Done"

# Build package manual (PDF)
manual:
    @just _msg "─── Building manual for {{pkg}}... ───"
    {{r}} CMD Rd2pdf . --output={{pkg}}.pdf
    @just _msg "Done"

# Build pkgdown site
site:
    @just _msg "─── Building pkgdown site for {{pkg}}... ───"
    {{rscript}} -e "pkgdown::build_site()"
    @just _msg "Done"

# Remove tarballs and .Rcheck output
clean:
    @just _msg "─── Cleaning build artifacts... ───"
    rm -rf {{pkg}}.Rcheck
    rm -f {{tarball_glob}}
    @just _msg "Done"
