# msg.R
# ::rtemis::
# 2016- EDG rtemis.org

# used by msgdatetime, log_to_file
datetime <- function(datetime_format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), datetime_format)
}

#' Message datetime()
#'
#' @param datetime_format Character: Format for the date and time.
#'
#' @return Character: Formatted date and time.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
# Used by msg(), msg0(), msgstart()
msgdatetime <- function(datetime_format = "%Y-%m-%d %H:%M:%S") {
  message(gray(paste0(datetime(), gray(" "))), appendLF = FALSE)
}


msg_info <- function(..., format_fn = highlight2) {
  msg0(..., format_fn = format_fn, caller_id = 2)
}

suggest <- function(...) {
  message <- paste(...)
  cat(highlight2(paste0("Suggestion: ", message, "\n")))
}

format_caller <- function(call_stack, call_depth, caller_id, max_char = 30L) {
  stack.length <- length(call_stack)
  if (stack.length < 2) {
    caller <- NA
  } else {
    call_depth <- call_depth + caller_id
    if (call_depth > stack.length) {
      call_depth <- stack.length
    }
    caller <- paste(
      lapply(
        rev(seq(call_depth)[-seq(caller_id)]),
        function(i) rev(call_stack)[[i]][[1]]
      ),
      collapse = ">>"
    )
  }
  # do.call and similar will change the call stack, it will contain the full
  # function definition instead of the name alone
  # Capture S7 method calls
  if (!is.na(caller) && substr(caller, 1, 8) == "`method(") {
    caller <- sub("`method\\(([^,]+),.*\\)`", "\\1", caller)
  }
  if (is.function(caller)) {
    # Try to get function name from call stack context
    caller <- tryCatch(
      {
        # Get the original call stack element as character
        call_str <- deparse(rev(call_stack)[[rev(seq(call_depth)[
          -seq(caller_id)
        ])[1]]])
        # Extract function name from the call
        fn_match <- regexpr("^[a-zA-Z_][a-zA-Z0-9_\\.]*", call_str)
        if (fn_match > 0) {
          regmatches(call_str, fn_match)
        } else {
          "(fn)"
        }
      },
      error = function(e) "(fn)"
    )
  }
  if (is.character(caller)) {
    if (nchar(caller) > 30) caller <- paste0(substr(caller, 1, 27), "...")
  }
  caller
} # / rtemis::format_caller


#' Message with provenance
#'
#' Print message to output with a prefix including data and time, and calling function or full
#' call stack
#'
#' If `msg` is called directly from the console, it will print `[interactive>]` in place of
#'   the call stack.
#' `msg0`, similar to `paste0`, is `msg(..., sep = "")`
#'
# Add following to each function using \code{msg}:
# \code{current <- as.list(sys.call())[[1]]}
#'
#' @param ... Message to print
#' @param date Logical: if TRUE, include date and time in the prefix
#' @param caller Character: Name of calling function
#' @param call_depth Integer: Print the system call path of this depth.
#' @param caller_id Integer: Which function in the call stack to print
#' @param newline_pre Logical: If TRUE begin with a new line.
#' @param newline Logical: If TRUE end with a new line.
#' @param color Color fn
#' @param sep Character: Use to separate objects in `...`
#'
#' @return Invisibly: List with call, message, and date
#' @author EDG
#'
#' @export
#' @keywords internal
#' @noRd
msg <- function(
  ...,
  date = rtemis_date,
  caller = NULL,
  call_depth = 1L,
  caller_id = 1L,
  newline_pre = FALSE,
  newline = TRUE,
  format_fn = plain,
  sep = " "
) {
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  } # / get caller

  txt <- Filter(Negate(is.null), list(...))
  if (newline_pre) {
    message("")
  }
  if (date) {
    msgdatetime()
  }
  message(
    format_fn(paste(txt, collapse = sep)),
    appendLF = FALSE
  )
  if (!is.null(caller) && !is.na(caller) && nchar(caller) > 0L) {
    message(plain(gray(paste0(" [", caller, "]"))))
  } else if (newline) {
    message("")
  }
} # rtemis::msg

#' @export
#' @keywords internal
#' @noRd
msg0 <- function(
  ...,
  caller = NULL,
  call_depth = 1,
  caller_id = 1,
  newline_pre = FALSE,
  newline = TRUE,
  format_fn = plain,
  sep = ""
) {
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  }

  txt <- Filter(Negate(is.null), list(...))
  if (newline_pre) {
    message("")
  }
  msgdatetime()
  message(
    format_fn(paste(txt, collapse = sep)),
    appendLF = FALSE
  )
  if (!is.null(caller) && !is.na(caller) && nchar(caller) > 0L) {
    message(plain(gray(paste0(" [", caller, "]"))))
  } else if (newline) {
    message("")
  }
} # rtemis::msg0


#' Pad-cat
#'
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' {
#'   msg("Hello")
#'   pcat("super", "wow")
#'   pcat(NULL, "oooo")
#' }
#' }
pcat <- function(left, right, pad = 17, newline = TRUE) {
  lpad <- max(0, pad - 1 - max(0, nchar(left)))
  cat(pad_string(left), right)
  if (newline) cat("\n")
}

pad_string <- function(x, target = 17, char = " ") {
  lpad <- max(0, target - max(0, nchar(x)))
  paste0(
    paste(rep(char, lpad), collapse = ""),
    x
  )
}


#' msgstart
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msgstart <- function(
  ...,
  newline_pre = FALSE,
  sep = ""
) {
  txt <- Filter(Negate(is.null), list(...))
  if (newline_pre) {
    message()
  }
  msgdatetime()
  message(plain(paste(txt, collapse = sep)), appendLF = FALSE)
} # rtemis::msgstart


#' msgdone
#'
#' @inheritParams msg
#'
#' @keywords internal
#' @noRd
msgdone <- function(caller = NULL, call_depth = 1, caller_id = 1, sep = " ") {
  if (is.null(caller)) {
    call_stack <- as.list(sys.calls())
    caller <- format_caller(call_stack, call_depth, caller_id)
  }
  message(" ", appendLF = FALSE)
  yay(end = "")
  message(gray(paste0("[", caller, "]\n")), appendLF = FALSE)
} # rtemis::msgdone
