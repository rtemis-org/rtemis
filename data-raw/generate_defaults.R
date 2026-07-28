# generate_defaults.R
# ::rtemis::
# 2026- EDG rtemis.org

# Emits the rtemis-wide defaults artifact: for every published schema, the
# default value each of its properties takes when the user supplies nothing.
#
# The schemas carry no `default` keyword; this artifact holds them instead,
# versioned independently and read by every implementation (R, the cli, the
# Python port).
#
# Defaults are read from the `setup_*()` formals, not `PropertySpec@default`.
# The two can differ: `ResamplerConfig@n_resamples` has class default NULL,
# while `setup_Resampler(n_resamples = 10L)` is what a user gets.
#
# Run with: Rscript data-raw/generate_defaults.R [SCHEMA_REPO]

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"

source(file.path("data-raw", "schema_registry.R"))


# %% .setup_fn ----
# The `setup_*()` function that builds `cls`, or NULL when the class has none
# (its defaults then come from nowhere a user can see, and it is skipped).
.setup_fn <- function(cls) {
  for (nm in doc_source_for_class(cls@name)) {
    fn <- tryCatch(get(nm, envir = asNamespace("rtemis")), error = function(e) {
      NULL
    })
    if (is.function(fn)) {
      return(fn)
    }
  }
  NULL
} # /.setup_fn


# %% .schema_properties ----
# Property names a generated schema declares, read from the schema itself so
# the artifact can never list a field the contract does not have.
.schema_properties <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  props <- jsonlite::fromJSON(path, simplifyVector = FALSE)[["properties"]]
  setdiff(names(props), "$schema")
} # /.schema_properties


# %% .formal_default ----
# Evaluate one `setup_*()` formal's default into a JSON-emittable value, or
# NULL when it has none / has no portable form.
#
# `spec` disambiguates a length > 1 default: for a vector-valued property the
# whole vector is the default (`date_features`), while for a scalar property a
# vector formal is the `match.arg()` idiom and the first element is what the
# user gets (`type = c("KFold", ...)`).
.formal_default <- function(value, env, spec) {
  if (missing(value) || identical(value, quote(expr = ))) {
    return(NULL)
  }
  out <- tryCatch(eval(value, envir = env), error = function(e) NULL)
  if (is.null(out) || !is.atomic(out) || length(out) == 0L) {
    # NULL, an S7 config object, a function: no portable default. Absence in
    # the artifact means "no default", which is what a consumer needs to know.
    return(NULL)
  }
  if (length(out) > 1L) {
    is_vector_valued <- !is.null(spec) && spec@container != "none"
    if (!is_vector_valued) {
      out <- out[[1L]]
    }
  }
  out
} # /.formal_default


# %% .class_defaults ----
# Defaults for the properties `schema_path` declares, drawn from `cls`'s
# `setup_*()` formals.
.class_defaults <- function(cls, schema_path) {
  fn <- .setup_fn(cls)
  if (is.null(fn)) {
    return(NULL)
  }
  wanted <- .schema_properties(schema_path)
  if (length(wanted) == 0L) {
    return(NULL)
  }
  fmls <- formals(fn)
  props <- cls@properties
  # Run state has no user-facing default: the run writes it, so whatever the
  # shared `setup_*()` formal says about it is not what a user gets. LOOCV is
  # the case that shows it -- `setup_Resampler(n_resamples = 10L)` serves all
  # six types, but LOOCV's count comes from the data.
  state <- role_prop_names(cls, "state")
  out <- list()
  for (nm in setdiff(intersect(wanted, names(fmls)), state)) {
    spec <- if (nm %in% names(props)) get_spec(props[[nm]]) else NULL
    value <- .formal_default(fmls[[nm]], environment(fn), spec)
    if (!is.null(value)) {
      out[[nm]] <- value
    }
  }
  if (length(out) == 0L) NULL else out
} # /.class_defaults


# Collect --------------------------------------------------------------------
defaults <- list()

for (family in names(families)) {
  fam <- families[[family]]
  discriminator <- if (is.null(fam[["discriminator"]])) {
    "algorithm"
  } else {
    fam[["discriminator"]]
  }
  for (algo in fam[["algorithms"]]) {
    cls <- algo[["cls"]]
    slug <- tolower(discriminator_value(cls, discriminator))
    id <- paste0(base_url, "/", family, "/", slug, "/v1/schema.json")
    path <- file.path(schema_repo, family, slug, "v1", "schema.json")
    d <- .class_defaults(cls, path)
    if (!is.null(d)) {
      defaults[[id]] <- d
    }
  }
  # A dispatcher's own properties are the discriminator plus any field declared
  # on the family base, which the variants' setup function also declares -- so
  # read them off the first variant.
  dispatcher_id <- paste0(base_url, "/", family, "/v1/schema.json")
  dispatcher_path <- file.path(schema_repo, family, "v1", "schema.json")
  d <- .class_defaults(fam[["algorithms"]][[1L]][["cls"]], dispatcher_path)
  if (!is.null(d)) {
    defaults[[dispatcher_id]] <- d
  }
}

for (family in names(flat_configs)) {
  cfg <- flat_configs[[family]]
  id <- paste0(base_url, "/", family, "/v1/schema.json")
  path <- file.path(schema_repo, family, "v1", "schema.json")
  d <- .class_defaults(cfg[["cls"]], path)
  if (!is.null(d)) {
    defaults[[id]] <- d
  }
}


# Write ----------------------------------------------------------------------
out_dir <- file.path(schema_repo, "defaults", "v1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "defaults.json")

# Keys sorted so the file diffs cleanly when a single default changes.
defaults <- defaults[order(names(defaults))]

json <- jsonlite::toJSON(
  list(
    `$id` = paste0(base_url, "/defaults/v1/defaults.json"),
    title = "rtemis defaults",
    description = paste0(
      "Default values for the properties of each schema at ",
      "schema.rtemis.org, as supplied by rtemis's `setup_*()` functions. ",
      "Annotations, not constraints: the schemas define what is valid, this ",
      "file what a user gets when they supply nothing. A property absent ",
      "here has no default. Versioned independently of the schemas, which ",
      "are immutable once published."
    ),
    defaults = defaults
  ),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  digits = NA
)
write_lines(
  as.character(json),
  file = out_file,
  overwrite = TRUE,
  verbosity = 0L
)

cat(sprintf(
  "defaults for %d schemas -> %s\n",
  length(defaults),
  out_file
))
