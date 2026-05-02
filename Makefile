.PHONY: format document install test site

# ── Format ───────────────────────────────────────────────────────────────────
format:
	@echo "=> Formatting rtemis"
	air format .

# ── Document ─────────────────────────────────────────────────────────────────
document: format
	@echo "=> Documenting rtemis"
	Rscript -e "roxygen2::roxygenize()"

# ── Install ──────────────────────────────────────────────────────────────────
install: document
	@echo "=> Installing rtemis"
	Rscript -e "pak::local_install()"

# ── Test ─────────────────────────────────────────────────────────────────────
test:
	@echo "=> Testing rtemis"
	Rscript -e "devtools::test(stop_on_failure = TRUE)"

# ── Build Site ───────────────────────────────────────────────────────────────
site:
	@echo "=> Building pkgdown site for rtemis"
	Rscript -e "pkgdown::build_site()"
