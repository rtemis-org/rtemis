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
rtemis_version <- utils::packageVersion("rtemis")

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
  # Set default options if not already set by user
  if (is.null(getOption("rtemis_theme"))) {
    options(rtemis_theme = "whitegrid")
  }
  if (is.null(getOption("rtemis_palette"))) {
    options(rtemis_palette = "rtms")
  }
  if (is.null(getOption("rtemis_font"))) {
    options(rtemis_font = "Helvetica")
  }
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    ncores <- default_n_workers(omit = 0L)
    vline <- paste0(
      "\n  .:",
      bold(pkgname),
      " v.",
      rtemis_version,
      " \U1F30A",
      " ",
      utils::sessionInfo()[[2]],
      " (",
      ncores,
      ngettext(ncores, " core", " cores"),
      " available)\n  "
    )
    packageStartupMessage(paste0(
      pkglogo(),
      vline,
      fmt_gradient(
        paste0(rep("\u2500", nchar(vline) - 14L), collapse = ""),
        colors = c(
          rtemis_colors[["red"]],
          rtemis_colors[["orange"]],
          rtemis_colors[["red"]]
        )
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
      " https://docs.rtemis.org/r/ml",
      "\n  \u2502 ",
      gray("Learn R:"),
      " https://pdsr.rtemis.org",
      "\n  \u2514    ",
      gray("Cite: "),
      rtcitation,
      "\n\n  ",
      fmt("PSA:", col = rtemis_colors[["red"]], bold = TRUE),
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
        utils::sessionInfo()[[2]]
      )
    )
  }
}
