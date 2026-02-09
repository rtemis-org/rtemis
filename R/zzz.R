# ▄▄▄▄  ▄▄▄▄▄▄▄▄ .• ▌ ▄ ·. ▪  .▄▄ ·
# ▀▄  █·•██  ▀▄.▀··██ ▐███▪██ ▐█ ▀.
# ▐▀▀▀▄  ▐█.▪▐▀▀▪▄▐█ ▌▐▌▐█·▐█·▄▀▀▀█▄
# ▐█•  █ ▐█▌·▐█▄▄▌██ ██▌▐█▌▐█▌▐█▄▪▐█
# .▀  ▀  ▀▀▀  ▀▀▀ ▀▀  █▪▀▀▀▀▀▀ ▀▀▀▀

# zzz.R
# ::rtemis::
# 2016- EDG rtemis.org

# rtemis internal environment
live <- new.env()
live[["parallelized_learners"]] <- c(
  "LightGBM",
  "LightRF",
  "LightRuleFit",
  "Ranger"
)

# vars
rtemis_version <- packageVersion("rtemis")
cores_available <- parallelly::availableCores()
cores_to_use <- max(cores_available - 3L, 1L)

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

# Progress reporting
setup_progress <- function() {
  progressr::handlers(global = TRUE)
  progressr::handlers(
    progressr::handler_cli(
      format = "{cli::pb_spin} [{pb_current}/{pb_total}] {pb_status}",
      format_done = "{cli::col_green(cli::symbol$tick)} Completed {pb_total} tasks",
      show_after = 0,
      clear = FALSE
    )
  )
}

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
  # setup_progress()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    # setup_progress()
    vline <- paste0(
      "\n  .:",
      bold(pkgname),
      " v.",
      rtemis_version,
      " \U1F30A",
      " ",
      sessionInfo()[[2]],
      " (",
      cores_available,
      " cores available)\n  "
    )
    packageStartupMessage(paste0(
      pkglogo(),
      vline,
      fmt_gradient(
        paste0(rep("\u2500", nchar(vline) - 13L), collapse = ""),
        colors = c(rt_red, rt_orange, rt_red)
      ),
      bold("\n  Defaults"),
      "\n  \u2502   ",
      gray("Theme: "),
      getOption("rtemis_theme", "whitegrid"),
      "\n  \u2502    ",
      gray("Font: "),
      getOption("rtemis_font", "Helvetica"),
      "\n  \u2514 ",
      gray("Palette: "),
      getOption("rtemis_palette", "rtms"),
      bold("\n  Resources"),
      "\n  \u2502    ",
      gray("Docs:"),
      " https://rdocs.rtemis.org",
      "\n  \u2502 ",
      gray("Learn R:"),
      " https://pdsr.rtemis.org",
      "\n  \u2514    ",
      gray("Cite: "),
      rtcitation,
      "\n\n  ",
      fmt("PSA:", col = rt_red, bold = TRUE),
      " Do not throw data at algorithms. Compute responsibly!"
    ))
  } else {
    packageStartupMessage(
      paste0(
        "  .:",
        pkgname,
        " ",
        rtemis_version,
        " \U1F30A",
        " ",
        sessionInfo()[[2]]
      )
    )
  }
}
