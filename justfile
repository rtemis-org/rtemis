# justfile
# ::rtemis::
# 2026- EDG rtemis.org

pkg := `awk '/^Package:/{print $2; exit}' DESCRIPTION`
r := env("R", "R")
rscript := env("RSCRIPT", "Rscript")
tarball_glob := pkg + "_*.tar.gz"
# Repos this package publishes into. No defaults: each is a working copy whose
# location is a property of the machine, not of the package, so set them in the
# environment (see __dev/ops.md). Recipes needing one fail naming it.
schema_repo := env("SCHEMA_REPO", "")
cli_repo := env("CLI_REPO", "")
live_repo := env("LIVE_REPO", "")

# List available recipes
default:
    @just --list

_msg msg:
    @printf '\033[38;2;108;163;160m[%s] %s\033[0m\n' "$(date '+%Y-%m-%d %H:%M:%S')" "{{ msg }}"

# Fail unless `path` names an existing directory, blaming the variable by name
_need var path:
    @if [ -z "{{ path }}" ]; then \
        echo "   Error: {{ var }} is not set. Point it at your local checkout (see __dev/ops.md)."; \
        exit 1; \
    elif [ ! -d "{{ path }}" ]; then \
        echo "   Error: {{ var }} is set to '{{ path }}', which is not a directory."; \
        exit 1; \
    fi

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

# Generate schemas + defaults into a throwaway directory to assert the config contract
schemas-check:
    @just _msg "─── Checking schema generation for {{ pkg }}... ───"
    @dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT; \
        {{ rscript }} data-raw/generate_schemas.R "$dir" && \
        {{ rscript }} data-raw/generate_defaults.R "$dir"
    @just _msg "Done"

# Write schemas + defaults to the schema repo (publishing step; commit there separately)
schemas repo=schema_repo:
    @just _need SCHEMA_REPO "{{ repo }}"
    @just _msg "─── Generating schemas for {{ pkg }} into {{ repo }}... ───"
    {{ rscript }} data-raw/generate_schemas.R {{ repo }}
    {{ rscript }} data-raw/generate_defaults.R {{ repo }}
    @just _msg "Done"

# Generate schemas and refresh the registry index; stops before the commit
publish-schemas: schemas
    @just _msg "─── Indexing {{ schema_repo }}... ───"
    cd "{{ schema_repo }}" && just index && just check
    @git -C "{{ schema_repo }}" status --short
    @just _msg "Review the diff above, then commit and push - the push is the deploy:"
    @echo "   git -C '{{ schema_repo }}' add -A && git -C '{{ schema_repo }}' commit -m 'add <Alg>' && git -C '{{ schema_repo }}' push"
    @echo "   just publish-status <alg>    # confirm it is live, then: just publish-downstream"

# Report where an algorithm stands in the publish chain (read-only)
publish-status alg:
    @just _msg "─── Publish status for {{ alg }} ───"
    @alg=$(printf '%s' "{{ alg }}" | tr '[:upper:]' '[:lower:]'); \
    state() { if [ -n "$2" ] && eval "$3" >/dev/null 2>&1; then echo "$4"; else echo "$5"; fi; }; \
    printf '  %-18s %s\n' "schema repo" \
        "$(state x "{{ schema_repo }}" '[ -d "{{ schema_repo }}/hyperparameters/'"$alg"'" ]' 'written' 'missing - run: just schemas')"; \
    printf '  %-18s %s\n' "local index" \
        "$(state x "{{ schema_repo }}" 'grep -q "hyperparameters/'"$alg"'/" "{{ schema_repo }}/index.json"' 'listed' 'NOT LISTED - run: just publish-schemas')"; \
    printf '  %-18s %s\n' "deployed" \
        "$(state x present 'curl -fsSL https://schema.rtemis.org/index.json | grep -q "hyperparameters/'"$alg"'/"' 'live' 'NOT DEPLOYED - commit + push in the schema repo')"; \
    printf '  %-18s %s\n' "cli schemas" \
        "$(state x "{{ cli_repo }}" '[ -d "{{ cli_repo }}/rtemis-cli/schemas/hyperparameters/'"$alg"'" ]' 'vendored' 'missing - run: just publish-downstream')"; \
    printf '  %-18s %s\n' "cli defaults" \
        "$(state x "{{ cli_repo }}" 'grep -q "hyperparameters/'"$alg"'/" "{{ cli_repo }}/rtemis-cli/defaults/defaults.json"' 'current' 'STALE - defaults are not refreshed by sync-schemas')"; \
    printf '  %-18s %s\n' "live schemas" \
        "$(state x "{{ live_repo }}" '[ -d "{{ live_repo }}/src/lib/rtemislive/schemas/hyperparameters/'"$alg"'" ]' 'vendored' 'missing - run: just publish-downstream')"
    @echo "   A running rtemis.server holds the rtemis it loaded at startup; restart it to pick this up."

# Vendor the deployed schemas into the CLI and live, and rebuild
publish-downstream:
    @just _need SCHEMA_REPO "{{ schema_repo }}"
    @just _need CLI_REPO "{{ cli_repo }}"
    @just _need LIVE_REPO "{{ live_repo }}"
    @just _msg "─── Vendoring into {{ live_repo }}... ───"
    cd "{{ live_repo }}" && pnpm sync:schemas && pnpm check:schemas && pnpm test
    @just _msg "─── Vendoring into {{ cli_repo }}... ───"
    cd "{{ cli_repo }}" && just sync-schemas
    cp "{{ schema_repo }}/defaults/v1/defaults.json" "{{ cli_repo }}/rtemis-cli/defaults/defaults.json"
    cd "{{ cli_repo }}" && just fbi
    @just _msg "─── Checking {{ cli_repo }} ───"
    @cd "{{ cli_repo }}" && just check || { \
        echo ""; \
        echo "   If the embedded-schema count assert failed, that is the tripwire working:"; \
        echo "   a new algorithm adds one input and one record schema, so raise both counts"; \
        echo "   in rtemis-cli/src/lib.rs by 1 and re-run. Do not loosen the assert."; \
        exit 1; \
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
