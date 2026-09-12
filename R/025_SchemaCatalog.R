# 025_SchemaCatalog.R
# ::rtemis::
# 2026- EDG rtemis.org

# The installed namespace is immutable. Development loads recreate this cache;
# explicit class inventories are always inspected independently.
.schema_catalog_cache <- new.env(parent = emptyenv())

# %% SchemaPublication ----
#' Publication metadata owned by one class
#'
#' @field role Character \{"family", "leaf", "document", "inline"\}: Publication role.
#' @field slug Optional Character: Explicit URL segment.
#' @field title Optional Character: Published title.
#' @field description Character: Published description.
#' @field discriminator Optional Character: Family dispatch property.
#' @field discriminator_description Optional Character: Dispatch property help.
#' @field order Integer [1, Inf): Presentation order within the publication role.
#' @field kind Character \{"config", "pipeline", "report", "component"\}: Document kind.
#' @field record_provenance,record_session Optional Character: Qualified class identities for record supplements.
#' @field record_folds Optional Character vector: Config properties resolved for each fold.
#' @field record_metrics Optional Character vector: Qualified metric class identities.
#' @field record_metrics_shape Character \{"object", "samples"\}: Metric storage shape.
#' @author EDG
#' @keywords internal
#' @noRd
SchemaPublication <- new_class(
  name = "SchemaPublication",
  package = "rtemis",
  properties = list(
    role = prop_string(
      "document",
      enum = c("family", "leaf", "document", "inline"),
      description = "Publication role of this class."
    ),
    slug = prop_string(
      NULL,
      nullable = TRUE,
      description = "Explicit URL segment when the class name does not determine it."
    ),
    title = prop_string(
      NULL,
      nullable = TRUE,
      description = "Published title when a distinct title is needed."
    ),
    description = prop_string(
      "",
      description = "Description of this class for schema consumers."
    ),
    discriminator = prop_string(
      NULL,
      nullable = TRUE,
      description = "Property that selects a family member."
    ),
    discriminator_description = prop_string(
      NULL,
      nullable = TRUE,
      description = "Description of the family selection property."
    ),
    order = prop_integer(
      1L,
      min = 1L,
      description = "Presentation order within a publication role."
    ),
    record_provenance = prop_string(
      NULL,
      nullable = TRUE,
      description = "Qualified provenance class carried by this pipeline's record."
    ),
    record_session = prop_string(
      NULL,
      nullable = TRUE,
      description = "Qualified class identifying the execution session sidecar."
    ),
    record_folds = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      unique_items = TRUE,
      description = "Config properties resolved independently for each outer fold."
    ),
    record_metrics = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      unique_items = TRUE,
      description = "Qualified classes of metrics produced by this pipeline."
    ),
    record_metrics_shape = prop_string(
      "object",
      enum = c("object", "samples"),
      description = "Whether metrics form one object or a map of samples with resampling summaries."
    ),
    kind = prop_string(
      "config",
      enum = c("config", "pipeline", "report", "component"),
      description = "Document behavior for input, record, or report generation."
    )
  ),
  validator = function(self) {
    supplements <- c(
      self@record_provenance,
      self@record_session,
      self@record_metrics
    )
    if (
      length(supplements) &&
        any(
          !grepl("^[A-Za-z][A-Za-z0-9.]*::[A-Za-z][A-Za-z0-9._]*$", supplements)
        )
    ) {
      return("record supplement targets must be qualified class identities.")
    }
    if (
      (length(supplements) || !is.null(self@record_folds)) &&
        self@kind != "pipeline"
    ) {
      return("record supplements require kind 'pipeline'.")
    }
    if (
      self@record_metrics_shape == "object" && length(self@record_metrics) > 1L
    ) {
      return("object metrics require one target class.")
    }
    if (!nzchar(self@description)) {
      return("@description must be non-empty.")
    }
    if (!is.null(self@title) && !nzchar(self@title)) {
      return("@title must be non-empty.")
    }
    if (!is.null(self@slug) && !grepl("^[a-z][a-z0-9_-]*$", self@slug)) {
      return("@slug must be a lowercase URL segment starting with a letter.")
    }
    if (self@role %in% c("inline", "leaf") && !is.null(self@slug)) {
      return("inline and leaf classes do not declare independent URL slugs.")
    }
    if (self@role == "family") {
      if (is.null(self@discriminator) || !nzchar(self@discriminator)) {
        return("a family must declare @discriminator.")
      }
      if (
        is.null(self@discriminator_description) ||
          !nzchar(self@discriminator_description)
      ) {
        return("a family must describe its discriminator.")
      }
    } else if (
      !is.null(self@discriminator) || !is.null(self@discriminator_description)
    ) {
      return("only a family may declare a discriminator.")
    }
    if (self@role != "document" && self@kind != "config") {
      return("family roots and leaves have kind 'config'.")
    }
    NULL
  }
)


# %% repr.SchemaPublication ----
#' @keywords internal
#' @noRd
method(repr, SchemaPublication) <- function(x, output_type = NULL, ...) {
  fmt(
    paste0("SchemaPublication: ", x@role, " (", x@kind, ")"),
    output_type = output_type
  )
}


# %% schema_class ----
#' Define an S7 class with its own publication contract
#'
#' @param ... Arguments: Passed to `S7::new_class()`.
#' @param publication Optional `SchemaPublication`: This class's publication metadata.
#' @param rules List of `SchemaRule` objects: Invariants declared by this class.
#' @param defaults Optional List: Named DefaultPolicy overrides for this class.
#' @return S7 class with validated, non-inheriting metadata.
#' @keywords internal
#' @noRd
schema_class <- function(
  ...,
  publication = NULL,
  rules = list(),
  defaults = NULL
) {
  if (!is.null(publication)) {
    check_is_S7(publication, SchemaPublication)
  }
  if (!is.list(rules)) {
    rtemis.core::abort(
      "`rules` must be a list of SchemaRule objects.",
      class = "rtemis_type_error"
    )
  }
  declared_rules <- lapply(rules, schema_rule_fields)
  args <- list(...)
  original_validator <- args[["validator"]]
  if (length(declared_rules)) {
    args[["validator"]] <- function(self) {
      c(
        if (!is.null(original_validator)) original_validator(self),
        validate_class_rules(self, declared_rules)
      )
    }
  }
  cls <- do.call(new_class, args)
  if (!is.null(defaults)) {
    if (
      !is.list(defaults) ||
        is.null(names(defaults)) ||
        anyDuplicated(names(defaults)) ||
        any(!names(defaults) %in% names(cls@properties))
    ) {
      rtemis.core::abort(
        "Class defaults must name distinct declared properties.",
        class = "rtemis_schema_error"
      )
    }
    for (policy in defaults) {
      check_is_S7(policy, DefaultPolicy)
    }
    attr(cls, "rtemis_defaults") <- lapply(defaults, props)
  }
  validate_default_policies(cls)
  validate_inherited_property_contracts(cls)
  if (length(declared_rules)) {
    attr(cls, "rtemis_rules") <- declared_rules
    attr(cls, "rtemis_native_validator") <- original_validator
  }
  inherited_rules <- schema_rules(cls)
  if (length(inherited_rules)) {
    ids <- vapply(inherited_rules, `[[`, character(1L), "id")
    if (anyDuplicated(ids)) {
      rtemis.core::abort(
        "Duplicate class rule identity in ",
        cls@name,
        ".",
        class = "rtemis_schema_error"
      )
    }
    for (rule in inherited_rules) {
      validate_rule_declaration(rule, cls)
    }
  }
  if (is.null(publication)) {
    return(cls)
  }
  for (nm in publication@record_folds) {
    if (is.null(get_spec_fields(cls@properties[[nm]])[["target_class"]])) {
      rtemis.core::abort(
        "Fold property ",
        nm,
        " must declare a class reference.",
        class = "rtemis_schema_error"
      )
    }
  }
  if (publication@role == "leaf" && isTRUE(cls@abstract)) {
    rtemis.core::abort(
      "A published leaf must be concrete: ",
      cls@name,
      ".",
      class = "rtemis_schema_error"
    )
  }
  if (
    publication@role == "family" &&
      !publication@discriminator %in% names(cls@properties)
  ) {
    rtemis.core::abort(
      "Family ",
      cls@name,
      " does not declare its discriminator.",
      class = "rtemis_schema_error"
    )
  }
  rtemis.core::assert_description_language(
    list(description = publication@description),
    cls@name
  )
  attr(cls, "rtemis_schema") <- S7::props(publication)
  cls
}


# %% property_validation_contract ----
#' Extract the inherited validation contract from property metadata
#' @param fields Optional named list: Property specification fields.
#' @return Named list excluding defaults and presentation metadata, or NULL.
#' @keywords internal
#' @noRd
property_validation_contract <- function(fields) {
  if (is.null(fields)) {
    return(NULL)
  }
  fields[c(
    "default",
    "default_present",
    "default_policy",
    "description",
    "group"
  )] <- NULL
  if (!is.null(fields[["items"]])) {
    fields[["items"]] <- property_validation_contract(fields[["items"]])
  }
  if (!is.null(fields[["members"]])) {
    fields[["members"]] <- lapply(
      fields[["members"]],
      property_validation_contract
    )
  }
  fields
}


# %% validate_inherited_property_contracts ----
#' Check property declarations against retained ancestor validators
#' @param cls S7 class: Class to inspect.
#' @return NULL, invisibly. Invalid overrides raise a schema error.
#' @keywords internal
#' @noRd
validate_inherited_property_contracts <- function(cls) {
  # S7 runs ancestor property validators on descendant instances as well.
  for (parent in schema_class_ancestors(cls)) {
    for (nm in names(parent@properties)) {
      inherited <- get_spec_fields(parent@properties[[nm]])
      if (is.null(inherited)) {
        next
      }
      declared <- get_spec_fields(cls@properties[[nm]])
      if (
        !identical(
          property_validation_contract(inherited),
          property_validation_contract(declared)
        )
      ) {
        rtemis.core::abort(
          cls@name,
          " has an unsupported type, value shape, or constraint override for inherited @",
          nm,
          "; keep its validation contract unchanged.",
          class = "rtemis_schema_error"
        )
      }
    }
  }
  invisible(NULL)
}


# %% schema_publication ----
#' Read publication metadata declared on exactly one class
#' @param cls S7 class: Class to inspect.
#' @return `SchemaPublication` or NULL when the class is unpublished.
#' @keywords internal
#' @noRd
schema_publication <- function(cls) {
  fields <- attr(cls, "rtemis_schema", exact = TRUE)
  if (is.null(fields)) NULL else do.call(SchemaPublication, fields)
}


# %% schema_native_validator ----
#' Read validation authored outside the portable rule declarations
#' @param cls S7 class: Declaring class, without inherited validators.
#' @return Function or NULL.
#' @keywords internal
#' @noRd
schema_native_validator <- function(cls) {
  if (length(attr(cls, "rtemis_rules", exact = TRUE))) {
    attr(cls, "rtemis_native_validator", exact = TRUE)
  } else {
    cls@validator
  }
}


# %% schema_class_ancestors ----
#' Read a class's S7 ancestors without constructing an instance
#' @param cls S7 class: Class to inspect.
#' @return List of ancestor class objects, nearest first.
#' @keywords internal
#' @noRd
schema_class_ancestors <- function(cls) {
  out <- list()
  cls <- cls@parent
  while (inherits(cls, "S7_class") && !identical(cls, S7::S7_object)) {
    out[[length(out) + 1L]] <- cls
    cls <- cls@parent
  }
  out
}


# %% schema_publication_annotation ----
#' Publish class identity and intent for catalog consumers
#' @param cls S7 class: Class to inspect.
#' @return Named list or NULL for an unpublished class.
#' @keywords internal
#' @noRd
schema_publication_annotation <- function(cls) {
  if (is.null(cls)) {
    return(NULL)
  }
  publication <- schema_publication(cls)
  if (is.null(publication)) {
    return(NULL)
  }
  list(
    producer = cls@package,
    class = paste0(cls@package, "::", cls@name),
    role = publication@role,
    kind = publication@kind,
    order = publication@order
  )
}


# %% schema_algorithm_descriptions ----
#' Read algorithm prose from the classes that publish it
#' @param base S7 class: Published family root.
#' @return Named character vector keyed by discriminator value.
#' @keywords internal
#' @noRd
schema_algorithm_descriptions <- function(base) {
  families <- Filter(
    function(f) identical(f[["base_class"]], base),
    schema_catalog()[["families"]]
  )
  if (length(families) != 1L) {
    rtemis.core::abort(
      "Algorithm descriptions require a published family.",
      class = "rtemis_schema_error"
    )
  }
  family <- families[[1L]]
  stats::setNames(
    vapply(family[["algorithms"]], `[[`, character(1L), "desc"),
    vapply(
      family[["algorithms"]],
      function(a) discriminator_value(a[["cls"]], family[["discriminator"]]),
      character(1L)
    )
  )
}


# %% schema_record_arguments ----
#' Resolve the record supplements declared on one pipeline class
#' @param cls S7 class: Pipeline class.
#' @param catalog Named list: Discovered publication catalog.
#' @param base_url Character: Schema publication base URL.
#' @return Named list of arguments for `S7_to_JSONSchema()`.
#' @keywords internal
#' @noRd
schema_record_arguments <- function(cls, catalog, base_url) {
  publication <- schema_publication(cls)
  urls <- schema_reference_urls(catalog, base_url, record = TRUE)
  resolve <- function(target) {
    if (is.null(target)) {
      return(NULL)
    }
    result <- unname(urls[target])
    if (anyNA(result)) {
      rtemis.core::abort(
        "Unpublished record supplement: ",
        paste(target[is.na(result)], collapse = ", "),
        ".",
        class = "rtemis_schema_error"
      )
    }
    result
  }
  folds <- publication@record_folds
  fold_refs <- if (!is.null(folds)) {
    stats::setNames(
      vapply(
        folds,
        function(nm) {
          resolve(get_spec_fields(cls@properties[[nm]])[["target_class"]])
        },
        character(1L)
      ),
      folds
    )
  }
  metrics <- resolve(publication@record_metrics)
  list(
    provenance_url = resolve(publication@record_provenance),
    session_url = resolve(publication@record_session),
    fold_refs = fold_refs,
    metrics_refs = if (publication@record_metrics_shape == "samples") metrics,
    metrics_ref = if (publication@record_metrics_shape == "object") metrics
  )
}


# %% schema_catalog ----
#' Discover the package's own explicitly published class declarations
#'
#' Discovery never constructs a class instance. Aliases are deduplicated by
#' class identity, and a family root does not opt descendants into publication.
#'
#' @param classes Optional named list: S7 classes to inspect; otherwise the
#' package namespace is inspected.
#' @return Named list with derived family and standalone publication entries.
#' @keywords internal
#' @noRd
schema_catalog <- function(classes = NULL) {
  cacheable <- is.null(classes) &&
    (isTRUE(.schema_catalog_cache[["ready"]]) ||
      environmentIsLocked(asNamespace("rtemis")))
  if (cacheable && !is.null(.schema_catalog_cache[["value"]])) {
    return(.schema_catalog_cache[["value"]])
  }
  if (is.null(classes)) {
    ns <- asNamespace("rtemis")
    classes <- mget(ls(ns, all.names = TRUE), ns, inherits = FALSE)
  }
  classes <- Filter(
    function(cls) {
      inherits(cls, "S7_class") &&
        identical(cls@package, "rtemis") &&
        !is.null(attr(cls, "rtemis_schema", exact = TRUE))
    },
    classes
  )
  unique_classes <- list()
  for (cls in classes) {
    previous <- unique_classes[[cls@name]]
    if (!is.null(previous) && !identical(previous, cls)) {
      rtemis.core::abort(
        "Ambiguous published class identity: ",
        cls@name,
        ".",
        class = "rtemis_schema_error"
      )
    }
    unique_classes[[cls@name]] <- cls
  }
  metadata <- lapply(unique_classes, schema_publication)
  ordered <- order(
    vapply(metadata, function(m) m@order, integer(1L)),
    names(metadata)
  )
  unique_classes <- unique_classes[ordered]
  metadata <- metadata[ordered]
  families <- list()
  documents <- list()
  inline <- list()
  root_names <- names(Filter(function(m) m@role == "family", metadata))
  slugs <- vapply(metadata, function(m) m@slug %||% "", character(1L))
  for (nm in names(unique_classes)) {
    cls <- unique_classes[[nm]]
    m <- metadata[[nm]]
    if (m@role == "inline") {
      inline[[paste0(cls@package, "::", cls@name)]] <- list(
        cls = cls,
        title = m@title %||% cls@name,
        description = m@description
      )
      next
    }
    if (m@role == "leaf") {
      next
    }
    slug <- m@slug %||% tolower(sub("Config$", "", cls@name))
    if (slug %in% c(names(families), names(documents))) {
      rtemis.core::abort(
        "Duplicate schema slug: ",
        slug,
        ".",
        class = "rtemis_schema_error"
      )
    }
    slugs[[nm]] <- slug
    if (m@role == "family") {
      families[[slug]] <- list(
        base_class = cls,
        title = m@title %||% paste0("rtemis ", cls@name),
        description = m@description,
        discriminator = m@discriminator,
        discriminator_description = m@discriminator_description,
        algorithms = list()
      )
    } else {
      documents[[slug]] <- list(
        cls = cls,
        title = m@title %||% paste0("rtemis ", cls@name),
        description = m@description,
        kind = m@kind
      )
    }
  }
  for (nm in names(unique_classes)) {
    if (metadata[[nm]]@role != "leaf") {
      next
    }
    cls <- unique_classes[[nm]]
    roots <- intersect(
      vapply(schema_class_ancestors(cls), function(a) a@name, character(1L)),
      root_names
    )
    if (length(roots) != 1L) {
      rtemis.core::abort(
        "Published leaf ",
        nm,
        " must belong to exactly one published family; found ",
        length(roots),
        ".",
        class = "rtemis_schema_error"
      )
    }
    slug <- slugs[[roots[[1L]]]]
    family <- families[[slug]]
    value <- discriminator_value(cls, family[["discriminator"]])
    existing <- vapply(
      family[["algorithms"]],
      function(a) {
        discriminator_value(a[["cls"]], family[["discriminator"]])
      },
      character(1L)
    )
    if (tolower(value) %in% tolower(existing)) {
      rtemis.core::abort(
        "Duplicate discriminator value or leaf URL in ",
        slug,
        ": ",
        value,
        ".",
        class = "rtemis_schema_error"
      )
    }
    family[["algorithms"]][[length(existing) + 1L]] <- list(
      cls = cls,
      title = metadata[[nm]]@title %||% paste0("rtemis ", cls@name),
      desc = metadata[[nm]]@description
    )
    families[[slug]] <- family
  }
  empty <- names(Filter(function(f) length(f[["algorithms"]]) == 0L, families))
  if (length(empty)) {
    rtemis.core::abort(
      "Published families without leaves: ",
      paste(empty, collapse = ", "),
      ".",
      class = "rtemis_schema_error"
    )
  }
  out <- list(families = families, flat_configs = documents, inline = inline)
  if (cacheable) {
    .schema_catalog_cache[["value"]] <- out
  }
  out
}
