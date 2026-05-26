PKG := $(shell awk '/^Package:/{print $$2; exit}' DESCRIPTION)
R ?= R
RSCRIPT ?= Rscript
CHECK_DIR := $(PKG).Rcheck
TARBALL_GLOB := $(PKG)_*.tar.gz

msg = @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$$(date '+%Y-%m-%d %H:%M:%S')" "$(1)"

.DEFAULT_GOAL := help

.PHONY: help format document install test build check check-cran check-cran-no-tests site clean

help:
	$(call msg,Available targets:)
	@printf '%s\n' \
		'  format      Format R code with air CLI (if available)' \
		'  document    Generate roxygen2 documentation' \
		'  install     Document and install the package locally with pak' \
		'  test        Run testthat::test_local(stop_on_failure = TRUE)' \
		'  build       Build the source tarball' \
		'  check       Run R CMD check on the built tarball' \
		'  check-cran  Run R CMD check --as-cran' \
		'  check-cran-no-tests  Run R CMD check --as-cran --no-tests' \
		'  manual      Build package manual' \
		'  site        Build pkgdown site' \
		'  clean       Remove tarballs and .Rcheck output'

format:
	$(call msg,─── Formatting $(PKG) package... ───)
	@if command -v air >/dev/null 2>&1; then \
		air format .; \
	else \
		echo "   Note: 'air' CLI not found — skipping R code formatting."; \
	fi
	$(call msg,Done)

document: format
	$(call msg,─── Documenting $(PKG) package... ───)
	$(RSCRIPT) -e "roxygen2::roxygenize()"
	$(call msg,Done)

install: document
	$(call msg,─── Installing $(PKG) package... ───)
	$(RSCRIPT) -e "pak::local_install(upgrade = TRUE)"
	$(call msg,Done)

test:
	$(call msg,─── Running testthat tests for $(PKG)... ───)
	$(RSCRIPT) -e "testthat::test_local(stop_on_failure = TRUE)"
	$(call msg,Done)

build: clean
	$(call msg,─── Building $(PKG) package... ───)
	$(R) CMD build .
	$(call msg,Done)

check: build
	$(call msg,─── Running R CMD check on $(PKG)... ───)
	$(R) CMD check $(TARBALL_GLOB)
	rm -f $(TARBALL_GLOB)
	$(call msg,Done)

check-cran: build
	$(call msg,─── Running R CMD check --as-cran on $(PKG)... ───)
	$(R) CMD check $(TARBALL_GLOB) --as-cran
	rm -f $(TARBALL_GLOB)
	$(call msg,Done)

check-cran-no-tests: build
	$(call msg,─── Running R CMD check --as-cran on $(PKG)... ───)
	$(R) CMD check $(TARBALL_GLOB) --as-cran --no-tests
	rm -f $(TARBALL_GLOB)
	$(call msg,Done)

manual:
	$(call msg,─── Building manual for $(PKG)... ───)
	$(R) CMD Rd2pdf . --output=$(PKG).pdf
	$(call msg,Done)

site:
	$(call msg,─── Building pkgdown site for $(PKG)... ───)
	$(RSCRIPT) -e "pkgdown::build_site()"
	$(call msg,Done)

clean:
	$(call msg,─── Cleaning build artifacts... ───)
	rm -rf $(CHECK_DIR)
	rm -f $(TARBALL_GLOB)
	$(call msg,Done)
