suppressMessages(library(rtemis.core)); library(S7)
mk <- function(p) tryCatch(new_class("Z", properties=list(x=p))()@x, error=function(e) "<error>")
cat("prop_string (nullable=TRUE)  default:", format(mk(prop_string(nullable=TRUE))), "\n")
cat("prop_float  (nullable=TRUE)  default:", format(mk(prop_float(nullable=TRUE))), "\n")
cat("prop_integer(nullable=TRUE)  default:", format(mk(prop_integer(nullable=TRUE))), "\n")
cat("prop_boolean(nullable=TRUE)  default:", format(mk(prop_boolean(nullable=TRUE))), "\n")
cat("prop_bag    (nullable=TRUE)  default:", format(mk(prop_bag(nullable=TRUE))), "\n\n")
cat("can prop_boolean express a NULL default? default=NULL ->",
    format(tryCatch(mk(prop_boolean(default=NULL, nullable=TRUE)), error=function(e) paste("ERR:", conditionMessage(e)))), "\n")
cat("\n-- spec defaults --\n")
for (nm in c("string","float","integer","boolean")) {
  p <- do.call(paste0("prop_", nm), list(nullable=TRUE))
  cat(sprintf("prop_%-8s spec$default: %s\n", nm, format(prop_spec(p)$default)))
}
