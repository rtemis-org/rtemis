# xtdescribe.R
# ::rtemis::
# 2024 EDG

#' Describe longitudinal dataset
#'
#' This function emulates the `xtdescribe` function in Stata.
#'
#' @param x data.frame: Longitudinal data with ID and time variables.
#' @param id_col Integer: The column position of the ID variable.
#' @param time_col Integer: The column position of the time variable.
#' @param n_patterns Integer: The number of patterns to display.
#'
#' @return data.frame: Summary of participation patterns, returned invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' # Load example longitudinal dataset
#' data(xt_example)
#'
#' # Describe the longitudinal structure
#' xtdescribe(xt_example)
xtdescribe <- function(
  x,
  id_col = 1,
  time_col = 2,
  n_patterns = 9
) {
  id_name <- names(x)[id_col]
  time_name <- names(x)[time_col]
  # Print vec_describe of ID, with n = number of unique IDs
  id_us <- sort(unique(x[[id_name]]))
  n_ids <- length(id_us)
  time_us <- sort(unique(x[[time_name]]))
  time_min <- min(time_us)
  time_max <- max(time_us)
  leftwidth <- max(nchar(id_name), nchar(time_name))
  cat(
    pad_string(id_name, leftwidth),
    ": ",
    vec_describe(id_us),
    "                         n = ",
    length(id_us),
    "\n",
    sep = ""
  )
  cat(
    pad_string(time_name, leftwidth),
    ": ",
    vec_describe(time_us),
    "                         T = ",
    length(time_us),
    "\n",
    sep = ""
  )
  # Calculate delta for time variable
  # ?is this minimum delta?
  delta <- min(diff(time_us))
  cat(
    strrep(" ", leftwidth),
    "  Delta (",
    time_name,
    ") = ",
    delta,
    " unit\n",
    sep = ""
  )
  span <- ((max(time_us) - min(time_us)) / delta) + 1
  cat(
    strrep(" ", leftwidth),
    "  Span (",
    time_name,
    ") = ",
    span,
    " periods\n",
    sep = ""
  )
  #  Does id * time have unique values?
  id_time_unique <- length(unique(interaction(x[[id_name]], x[[time_name]]))) ==
    nrow(x)
  uid <- if (id_time_unique) {
    "uniquely identifies"
  } else {
    "does not uniquely identify"
  }
  cat(
    strrep(" ", leftwidth),
    "  (",
    id_name,
    "*",
    time_name,
    " ",
    uid,
    " each observation)\n",
    sep = ""
  )
  # Distribution of T_i at min, 5%, 25%, 50%, 75%, 95%, max
  cat(
    "\nDistribution of T_i:",
    "\tmin\t5%\t25%\t50%\t75%\t95%\tmax\n",
    sep = ""
  )
  id_freq <- table(x[[id_name]])
  id_freq_quant <- quantile(id_freq, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
  cat(
    strrep(" ", 20),
    "\t",
    paste(id_freq_quant, collapse = "\t"),
    "\n",
    sep = ""
  )

  # Participation pattern by time
  # Get N IDs per time point, calculate pct of total IDs and rank
  id_time_freq <- table(x[[id_name]], x[[time_name]])
  # Insert columns of 0s for missing time points
  # id_time_freq <- cbind(id_time_freq, matrix(0, nrow = nrow(id_time_freq), ncol = span - ncol(id_time_freq)))
  # Add column names for missing time points
  missing_time_points <- setdiff(seq(time_min, time_max, by = delta), time_us)
  missing <- matrix(
    0,
    nrow = nrow(id_time_freq),
    ncol = length(missing_time_points)
  )
  colnames(missing) <- missing_time_points
  id_time_freq <- cbind(id_time_freq, missing)
  # Re order columns by name
  id_time_freq <- id_time_freq[, order(as.numeric(colnames(id_time_freq)))]

  # Convert to pattern matrix by pasting all columns by row
  # id_time_freq_char <- as.matrix(id_time_freq)
  # id_time_freq_char[id_time_freq_char == 0] <- "."
  id_time_freq_char <- matrix(
    as.character(id_time_freq),
    nrow = nrow(id_time_freq)
  )
  id_time_freq_char[id_time_freq_char == "0"] <- "."
  id_time_freq_pattern <- apply(id_time_freq_char, 1, paste, collapse = "")
  id_time_freq_pattern_freq <- table(id_time_freq_pattern)
  id_time_freq_pattern_sorted <- sort(
    id_time_freq_pattern_freq,
    decreasing = TRUE
  )

  # Make data.frame with Frequency, Percent, Cumulative Percent of top n_patterns and rest
  pattern_summary <- data.frame(
    `Freq.` = as.numeric(id_time_freq_pattern_sorted)[seq_len(n_patterns)]
  )
  pattern_summary[["Percent"]] <- round(
    (pattern_summary[["Freq."]] / n_ids) * 100,
    digits = 2
  )
  pattern_summary[["Cum."]] <- cumsum(pattern_summary[["Percent"]])
  pattern_summary[["Pattern"]] <- names(id_time_freq_pattern_sorted)[seq_len(
    n_patterns
  )]
  # Add Freq, Percent, Cumulative Percent of rest
  pattern_summary <- rbind(
    pattern_summary,
    data.frame(
      `Freq.` = sum(id_time_freq_pattern_sorted[-seq_len(n_patterns)]),
      Percent = round(
        (sum(id_time_freq_pattern_sorted[-seq_len(n_patterns)]) / n_ids) * 100,
        digits = 2
      ),
      `Cum.` = "100.00",
      Pattern = "(other patterns)"
    )
  )
  # Missing pattern is X for time points with data and . for time points with no data
  missing_pattern <- rep("X", ncol(id_time_freq))
  missing_pattern[colSums(id_time_freq) == 0] <- "."
  missing_pattern <- paste(missing_pattern, collapse = "")
  # Add row with Total
  pattern_summary <- rbind(
    pattern_summary,
    data.frame(
      `Freq.` = n_ids,
      Percent = "100.00",
      `Cum.` = "",
      Pattern = missing_pattern
    )
  )

  print(pattern_summary, row.names = FALSE)
  invisible(pattern_summary)
} # /rtemis::xtdescribe


#' @keywords internal
#' @noRd
vec_describe <- function(x, sort_unique = FALSE) {
  # sort_unique defaults to FALSE since it needs to be computed already
  # within xtdescribe
  xs <- if (sort_unique) sort(unique(x)) else x
  paste(xs[1], xs[2], "...", xs[length(xs)], sep = ", ")
}
