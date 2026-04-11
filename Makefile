.PHONY: document install test site

# ── Document ─────────────────────────────────────────────────────────────────
document:
	@echo "=> Documenting rtemis"
	Rscript -e "devtools::document()"

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
