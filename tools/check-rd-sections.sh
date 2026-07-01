#!/bin/sh

# check-rd-sections.sh
# Report which .Rd files in a man/ directory are missing \value or \examples.
# Uses tools::parse_Rd so it checks the parsed Rd tags, not literal strings.
#
# Vendored into the repo so `just check-rd` works for any contributor without
# an external checkout. Source of truth: shell-utils/bin/check-rd-sections.sh.

# Timestamped message helper (inlined from shell-utils/lib/msg.sh).
msg() {
    msg_text=$1
    msg_color=${2:-${MSG_COLOR:-"1;38;2;0;153;107"}}
    msg_ts_color=${MSG_TS_COLOR:-"38;5;244"}

    printf '\033[%sm%s\033[%sm %s\n\033[m' \
        "$msg_ts_color" \
        "$(date +"%Y-%m-%d %H:%M:%S")" \
        "$msg_color" \
        "$msg_text"
}

MAN_DIR="man"
STRICT=0
INTERNAL=0

while [ "$#" -gt 0 ]; do
    case "$1" in
        -strict)
            # Also flag data/package/internal docs that omit \value or \examples.
            STRICT=1
            shift
            ;;
        -internal)
            # Also flag \keyword{internal} docs (data/package stay exempt).
            INTERNAL=1
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [-strict] [-internal] [MAN_DIR]"
            echo "  MAN_DIR     Path to a package's man/ directory (default: ./man)"
            echo "  -internal   Enforce \\keyword{internal} docs (data/package stay exempt)"
            echo "  -strict     Do not exempt any docs (data/package/internal)"
            echo "Exit status: 0 if nothing missing, 1 if any file is missing a section."
            exit 0
            ;;
        *)
            MAN_DIR="$1"
            shift
            ;;
    esac
done

if [ ! -d "$MAN_DIR" ]; then
    msg "Directory not found: $MAN_DIR" "1;38;2;220;60;60"
    exit 2
fi

msg "Checking Rd sections in $MAN_DIR ..."

MAN_DIR="$MAN_DIR" STRICT="$STRICT" INTERNAL="$INTERNAL" Rscript --vanilla - <<'RSCRIPT'
man_dir <- Sys.getenv("MAN_DIR")
strict <- Sys.getenv("STRICT") == "1"
check_internal <- Sys.getenv("INTERNAL") == "1"

files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
if (!length(files)) {
  cat("No .Rd files found.\n")
  quit(status = 0)
}

# ANSI helpers
red   <- function(x) paste0("\033[38;2;220;60;60m",  x, "\033[m")
green <- function(x) paste0("\033[38;2;0;153;107m",  x, "\033[m")
grey  <- function(x) paste0("\033[38;5;244m",        x, "\033[m")

top_tags <- function(rd) unique(vapply(rd, attr, "", "Rd_tag"))

# Pull the text of a \docType, if any, to exempt data/package docs from \value.
doc_type <- function(rd) {
  for (el in rd) {
    if (identical(attr(el, "Rd_tag"), "\\docType")) {
      return(trimws(paste(unlist(el), collapse = "")))
    }
  }
  ""
}

n_missing <- 0L
for (f in sort(files)) {
  rd <- tryCatch(
    tools::parse_Rd(f, permissive = TRUE),
    error = function(e) NULL
  )
  if (is.null(rd)) {
    cat(red(sprintf("  PARSE ERROR  %s\n", basename(f))))
    n_missing <- n_missing + 1L
    next
  }

  tags <- top_tags(rd)
  dtype <- doc_type(rd)

  has_value    <- "\\value"    %in% tags
  has_examples <- "\\examples" %in% tags

  # Categorise docs where \value / \examples are conventionally absent.
  # "import" is roxygen's docType for re-export stubs (man/reexports.Rd).
  is_data_pkg <- dtype %in% c("data", "package", "import")
  is_internal <- "\\keyword" %in% tags &&
    any(vapply(rd, function(el)
      identical(attr(el, "Rd_tag"), "\\keyword") &&
        grepl("internal", paste(unlist(el), collapse = "")),
      logical(1)))

  exempt <- FALSE
  if (!strict) {
    if (is_data_pkg) exempt <- TRUE
    if (is_internal && !check_internal) exempt <- TRUE
  }

  miss <- character(0)
  if (!has_value)    miss <- c(miss, "\\value")
  if (!has_examples) miss <- c(miss, "\\examples")

  if (!length(miss)) {
    cat(green(sprintf("  OK           %s\n", basename(f))))
  } else if (exempt) {
    cat(grey(sprintf("  SKIP (%-7s) %s  [missing: %s]\n",
                     if (nzchar(dtype)) dtype else "internal",
                     basename(f), paste(miss, collapse = ", "))))
  } else {
    cat(red(sprintf("  MISSING      %s  [%s]\n", basename(f), paste(miss, collapse = ", "))))
    n_missing <- n_missing + 1L
  }
}

cat(sprintf("\n%d file(s) checked, %s\n", length(files),
            if (n_missing) red(sprintf("%d with missing sections", n_missing))
            else green("all complete")))
quit(status = if (n_missing) 1L else 0L)
RSCRIPT
STATUS=$?

if [ "$STATUS" -eq 0 ]; then
    msg "Done: all Rd files have \\value and \\examples."
else
    msg "Some Rd files are missing sections (exit $STATUS)." "1;38;2;220;60;60"
fi
exit "$STATUS"
