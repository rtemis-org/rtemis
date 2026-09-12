# 027_DefaultArtifact.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% default_artifact_graph ----
#' Reconstruct typed references from a closed artifact graph
#' @param schemas Named list: Parsed schemas keyed by canonical identity.
#' @param defaults Named list: Versioned defaults artifact.
#' @param authoring Optional List: Authoring maps keyed by schema identity.
#' @return List containing class and wire-value readers backed only by artifacts.
#' @keywords internal
#' @noRd
default_artifact_graph <- function(schemas, defaults, authoring = NULL) {
  if (!identical(defaults[["format_version"]], 2L)) {
    rtemis.core::abort(
      "Unsupported defaults artifact version.",
      class = "rtemis_schema_error"
    )
  }
  classes <- new.env(parent = emptyenv())
  active <- character()
  identities <- locations <- parents <- list()
  visit <- function(node, id, path = "") {
    if (!is.list(node)) {
      return(invisible(NULL))
    }
    publication <- node[["x-rtemis"]][["publication"]]
    if (!is.null(publication[["class"]])) {
      identity <- publication[["class"]]
      if (is.null(identities[[identity]])) {
        identities[[identity]] <<- list(id = id, path = path, schema = node)
      }
      locations[[paste0(id, "#", path)]] <<- identity
      if (identical(publication[["role"]], "family")) {
        for (branch in node[["allOf"]]) {
          ref <- branch[["then"]][["$ref"]]
          if (!is.null(ref)) {
            parents[[ref]] <<- list(
              identity = identity,
              selector = branch[["if"]][["properties"]]
            )
          }
        }
      }
    }
    for (i in seq_along(node)) {
      key <- if (is.null(names(node))) {
        as.character(i - 1L)
      } else {
        default_pointer(names(node)[[i]])
      }
      visit(node[[i]], id, paste0(path, "/", key))
    }
  }
  # Config and record documents may share a class identity. Declaration
  # ownership selects its definition independently of graph ordering.
  for (id in intersect(names(schemas), names(defaults[["declarations"]]))) {
    visit(schemas[[id]], id)
  }
  decode <- function(value, target) {
    location <- identities[[target]]
    if (is.null(location)) {
      rtemis.core::abort(
        "Unavailable reference target: ",
        target,
        ".",
        class = "rtemis_schema_error"
      )
    }
    schema <- location[["schema"]]
    if (identical(schema[["x-rtemis"]][["publication"]][["role"]], "family")) {
      matches <- Filter(
        function(branch) {
          selector <- branch[["if"]][["properties"]]
          length(selector) > 0L &&
            all(vapply(
              names(selector),
              function(nm) {
                identical(value[[nm]], selector[[nm]][["const"]])
              },
              logical(1L)
            ))
        },
        schema[["allOf"]]
      )
      if (length(matches) != 1L) {
        rtemis.core::abort(
          "A referenced family requires one explicit discriminator: ",
          target,
          ".",
          class = "rtemis_schema_error"
        )
      }
      target <- locations[[paste0(matches[[1L]][["then"]][["$ref"]], "#")]]
    }
    cls <- class_for(target)
    value[["$schema"]] <- NULL
    unknown <- setdiff(names(value), names(cls@properties))
    if (length(unknown)) {
      rtemis.core::abort(
        "Unknown reference fields: ",
        paste(unknown, collapse = ", "),
        class = "rtemis_schema_error"
      )
    }
    document <- attr(cls, "rtemis_artifact_schema")
    args <- lapply(names(value), function(nm) {
      default_from_wire(value[[nm]], document[["properties"]][[nm]], decode)
    })
    names(args) <- names(value)
    do.call(cls, args)
  }
  class_for <- function(identity) {
    if (exists(identity, classes, inherits = FALSE)) {
      return(get(identity, classes, inherits = FALSE))
    }
    if (identity %in% active) {
      rtemis.core::abort(
        "Cyclic default expansion at ",
        identity,
        ".",
        class = "rtemis_schema_error"
      )
    }
    location <- identities[[identity]]
    if (is.null(location)) {
      rtemis.core::abort(
        "Unavailable artifact class: ",
        identity,
        ".",
        class = "rtemis_schema_error"
      )
    }
    active <<- c(active, identity)
    on.exit(active <<- setdiff(active, identity))
    id <- location[["id"]]
    path <- location[["path"]]
    schema <- location[["schema"]]
    declaration <- defaults[["declarations"]][[id]]
    if (identical(schema[["x-rtemis"]][["publication"]][["role"]], "family")) {
      selectors <- unique(unlist(
        lapply(schema[["allOf"]], function(branch) {
          names(branch[["if"]][["properties"]])
        }),
        use.names = FALSE
      ))
      schema[["properties"]][selectors] <- NULL
    }
    if (nzchar(path)) {
      keep <- startsWith(names(declaration), paste0(path, "/"))
      declaration <- declaration[keep]
      names(declaration) <- substring(names(declaration), nchar(path) + 1L)
    }
    parent <- NULL
    parent_info <- parents[[id]]
    if (!is.null(parent_info)) {
      parent_location <- identities[[parent_info[["identity"]]]]
      parent_schema <- parent_location[["schema"]]
      schema[["properties"]] <- c(
        parent_schema[["properties"]],
        schema[["properties"]]
      )
      declaration <- c(
        defaults[["declarations"]][[parent_location[["id"]]]],
        declaration
      )
      # A minimal ancestor supplies type identity; all effective property
      # declarations and constructor arguments belong to the concrete class.
      parts <- strsplit(parent_info[["identity"]], "::", fixed = TRUE)[[1L]]
      parent <- new_class(parts[[2L]], package = parts[[1L]])
      for (nm in names(parent_info[["selector"]])) {
        property <- prop_const(parent_info[["selector"]][[nm]][["const"]])
        schema[["properties"]][[nm]] <- prop_to_schema(property)
        declaration <- c(
          declaration,
          default_declarations(
            get_spec(property),
            schema[["properties"]][[nm]],
            paste0("/properties/", default_pointer(nm))
          )
        )
      }
    }
    # Concrete branches carry their constant discriminator. Other fields
    # require a property annotation for declaration reconstruction.
    schema[["properties"]] <- Filter(
      function(p) !is.null(p[["x-rtemis"]][["type"]]),
      schema[["properties"]]
    )
    parts <- strsplit(identity, "::", fixed = TRUE)[[1L]]
    policies <- defaults[["resolution"]][[paste0(
      id,
      if (nzchar(path)) paste0("#", path) else ""
    )]] %||%
      list()
    cls <- JSONSchema_to_S7(
      schema,
      name = parts[[2L]],
      package = parts[[1L]],
      declarations = declaration,
      authoring = authoring[[id]],
      decode_reference = decode,
      parent = parent,
      policies = policies
    )
    attr(cls, "rtemis_artifact_schema") <- schema
    assign(identity, cls, classes)
    cls
  }
  list(
    class = function(id) {
      identity <- locations[[paste0(
        id,
        if (grepl("#", id, fixed = TRUE)) "" else "#"
      )]]
      if (is.null(identity)) {
        rtemis.core::abort(
          "Unknown artifact schema: ",
          id,
          ".",
          class = "rtemis_schema_error"
        )
      }
      class_for(identity)
    },
    decode = decode
  )
}


# %% default_schema_bundle ----
#' Bundle actual referenced contracts for offline literal validation
#' @param schemas Named list: Parsed schema documents keyed by ID.
#' @param root List: Root property or object contract.
#' @param owner Character: Identity containing the root.
#' @return JSON-ready schema with local references.
#' @keywords internal
#' @noRd
default_schema_bundle <- function(schemas, root, owner) {
  keys <- stats::setNames(paste0("s", seq_along(schemas)), names(schemas))
  included <- character()
  defs <- list()
  localize <- function(node, id) {
    if (!is.list(node)) {
      return(node)
    }
    ref <- node[["$ref"]]
    if (!is.null(ref)) {
      target <- if (startsWith(ref, "#")) id else sub("#.*$", "", ref)
      if (!target %in% names(schemas)) {
        rtemis.core::abort(
          "Unavailable schema reference: ",
          ref,
          class = "rtemis_schema_error"
        )
      }
      fragment <- if (grepl("#", ref, fixed = TRUE)) {
        sub("^[^#]*#", "", ref)
      } else {
        ""
      }
      node[["$ref"]] <- paste0("#/$defs/", keys[[target]], fragment)
      if (!target %in% included) {
        included <<- c(included, target)
        document <- schemas[[target]]
        document[["$id"]] <- NULL
        defs[[keys[[target]]]] <<- localize(document, target)
      }
    }
    lapply(node, localize, id = id)
  }
  root <- localize(root, owner)
  if (length(defs)) {
    root[["$defs"]] <- defs
  }
  root[["$schema"]] <- "https://json-schema.org/draft/2020-12/schema"
  root
}
