# draw_linad.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% linad_diverging ----
#' Map values to a diverging color scale centered on zero
#'
#' Centered on zero deliberately, and never on the data's own midpoint: the
#' thing a reader is looking for is a coefficient changing **sign** between
#' nodes, and that only reads as a color flip if zero is the pivot.
#'
#' The range is taken across every value passed in, so one call must cover every
#' table that will be shown together. Coloring each node's table against its
#' own range would make the same coefficient a different color in each, which
#' is precisely the comparison the plot exists to support.
#'
#' @param x Numeric vector: Values, from every table at once.
#' @param lo_col Character: Color for the most negative value.
#' @param mid_col Character: Color at zero.
#' @param hi_col Character: Color for the most positive value.
#' @param n Integer: Gradient resolution.
#'
#' @return Character vector of colors, parallel to `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_diverging <- function(
  x,
  lo_col,
  mid_col,
  hi_col,
  n = 201L
) {
  gradient <- colorgrad(n = n, lo = lo_col, mid = mid_col, hi = hi_col)
  extreme <- max(abs(x[is.finite(x)]), na.rm = TRUE)
  if (!is.finite(extreme) || extreme == 0) {
    return(rep(gradient[[(n + 1L) %/% 2L]], length(x)))
  }
  # Anchoring the cut points at -extreme and +extreme is what puts zero in the
  # middle bin; cutting `x` alone would place the midpoint wherever the data
  # happened to sit.
  index <- cut(c(-extreme, x, extreme), n, labels = FALSE)
  gradient[index[seq.int(2L, length(x) + 1L)]]
} # /rtemis::linad_diverging


# %% linad_node_rule ----
#' The split condition leading into a node, as text
#'
#' Read off the frame rather than rebuilt from stored rule strings: the frame
#' carries `parent`, `left`, `split_feature`, `split_value` and `split_levels`,
#' so which side a node is on and what condition put it there are both known.
#'
#' @param frame data.table: The tree frame.
#' @param node Integer: Node id.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_node_rule <- function(frame, node) {
  parent <- frame[["parent"]][[node]]
  if (is.na(parent)) {
    return("All cases")
  }
  feature <- frame[["split_feature"]][[parent]]
  goes_left <- identical(frame[["left"]][[parent]], node)
  if (identical(frame[["split_kind"]][[parent]], "factor")) {
    levels_left <- frame[["split_levels"]][[parent]]
    if (goes_left) {
      paste0(feature, " = ", paste(levels_left, collapse = ", "))
    } else {
      paste0(feature, " != ", paste(levels_left, collapse = ", "))
    }
  } else {
    paste(
      feature,
      if (goes_left) "<" else ">=",
      ddSci(frame[["split_value"]][[parent]], 3L)
    )
  }
} # /rtemis::linad_node_rule


# %% draw_linad ----
#' Plot a Linear Additive Tree
#'
#' Draw a fitted LINAD model as an interactive tree: the partition as a
#' hierarchy, each node's value on its label, and each node's linear
#' coefficients in a table on hover.
#'
#' @details
#' The coefficient tables are the point. A LINAD leaf's coefficients are the
#' accumulated sum along its path, so the same feature can carry a different
#' slope in each region of the data -- and sometimes a different **sign**. That
#' is the model's central interpretive claim, and it is only visible by
#' comparing the table at one node against the table at another. The color
#' scale is therefore
#' diverging and centered on zero, and computed once across every table, so a
#' sign change reads as a color flip rather than as a number to be parsed.
#'
#' Each node's label carries `node_value`, what the **tree alone** predicts
#' there, while the table on hover carries what the **linear model** adds. The
#' two are separate quantities in a fitted LINAD, so the plot shows them
#' separately: with `node_model = "constant"` the tables are empty of slopes and
#' the node values are the entire model, which is what a plain decision tree
#' looks like.
#'
#' The intercept is excluded from the color range. It is typically far larger
#' than any slope and would compress every other coefficient into the middle of
#' the scale.
#'
#' This is the reference implementation for the LINAD tree plot, built on
#' `visNetwork` because a tooltip carrying a real HTML table needs a backend
#' whose tooltip is an HTML element. Plotly, which every other `draw_*` uses,
#' renders hover labels as SVG and cannot express a table at all.
#'
#' @param x `Regression` or `Classification` object holding a LINAD model, or a
#' fitted LINAD model itself.
#' @param n_leaves Optional Integer: Draw the tree at this size. NULL draws the
#' size selected at training.
#' @param top Optional Integer: Show only this many coefficients per node,
#' largest in absolute value first.
#' @param sort_coefs Logical: If TRUE, order each table by absolute coefficient.
#' @param direction Character \{"UD", "DU", "LR", "RL"\}: Tree direction.
#' @param node_labels Logical: If TRUE, label nodes with their rule.
#' @param show_ncases Logical: If TRUE, include the case count in node labels.
#' @param show_node_value Logical: If TRUE, include the node's own value in its
#' label.
#' @param lo_col Character: Color for the most negative coefficient.
#' @param mid_col Character: Color at zero.
#' @param hi_col Character: Color for the most positive coefficient.
#' @param root_col Character: Root node color.
#' @param node_col Character: Internal node color.
#' @param leaf_col Character: Terminal node color.
#' @param theme `Theme` object.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @return `visNetwork` htmlwidget.
#'
#' @author EDG
#' @export
#' @examplesIf interactive() && requireNamespace("visNetwork", quietly = TRUE)
#' x <- data.frame(a = rnorm(300), b = rnorm(300))
#' x$y <- 2 * x$a + ifelse(x$b < 0, -3, 3) + rnorm(300)
#' mod <- train(x, hyperparameters = setup_LINAD(max_leaves = 5L))
#' draw_linad(mod)
draw_linad <- function(
  x,
  n_leaves = NULL,
  top = NULL,
  sort_coefs = TRUE,
  direction = "UD",
  node_labels = TRUE,
  show_ncases = TRUE,
  show_node_value = TRUE,
  lo_col = "#0290EE",
  mid_col = "#1A1A1A",
  hi_col = "#FE4AA3",
  root_col = "#7F7F7F",
  node_col = "#404040",
  leaf_col = "#16A0AC",
  theme = choose_theme(getOption("rtemis_theme")),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("visNetwork")

  # Arguments ----
  check_is_S7(theme, Theme)
  direction <- match.arg(direction, c("UD", "DU", "LR", "RL"))
  model <- if (inherits(x, "rtemis::LinearAdditiveTree")) {
    x
  } else if (
    inherits(x, "rtemis::Supervised") ||
      inherits(x, "rtemis::Regression") ||
      inherits(x, "rtemis::Classification")
  ) {
    x@model
  } else {
    NULL
  }
  if (!inherits(model, "rtemis::LinearAdditiveTree")) {
    rtemis.core::abort(
      "draw_linad() needs a LINAD model; got ",
      class(x)[[1L]],
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (is.null(n_leaves)) {
    n_leaves <- model@n_leaves
  }
  if (n_leaves < 1L || n_leaves > length(model@steps)) {
    rtemis.core::abort(
      "n_leaves must be between 1 and ",
      length(model@steps),
      ", the sizes this tree passed through.",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }

  # Tree at the requested size ----
  # `is_leaf` describes the size selected at training, so the terminal set is
  # read from `steps` instead and any node below it is dropped.
  terminal <- model@steps[[n_leaves]]
  frame <- model@frame
  shown <- sort(unique(c(
    terminal,
    unlist(lapply(terminal, function(node) {
      ancestors <- integer(0)
      parent <- frame[["parent"]][[node]]
      while (!is.na(parent)) {
        ancestors <- c(ancestors, parent)
        parent <- frame[["parent"]][[parent]]
      }
      ancestors
    }))
  )))

  # Coefficient tables ----
  coefficients <- model@coefficients[shown, , drop = FALSE]
  labels <- colnames(coefficients)
  labels[[1L]] <- "(Int)"
  tables <- lapply(seq_along(shown), function(i) {
    values <- coefficients[i, ]
    index <- if (sort_coefs) {
      c(1L, order(abs(values[-1L]), decreasing = TRUE) + 1L)
    } else {
      seq_along(values)
    }
    if (!is.null(top)) {
      index <- index[seq_len(min(length(index), top + 1L))]
    }
    data.frame(Term = labels[index], Coefficient = unname(values[index]))
  })
  # One gradient across every table, excluding intercepts: they dwarf the
  # slopes and would flatten the scale everything else is read on.
  slopes <- unlist(lapply(tables, function(tab) tab[["Coefficient"]][-1L]))
  slope_cols <- linad_diverging(slopes, lo_col, mid_col, hi_col)
  offsets <- cumsum(c(
    0L,
    vapply(tables, function(tab) NROW(tab) - 1L, integer(1L))
  ))
  tooltips <- vapply(
    seq_along(tables),
    function(i) {
      taken <- if (NROW(tables[[i]]) > 1L) {
        slope_cols[seq.int(offsets[[i]] + 1L, offsets[[i + 1L]])]
      } else {
        character(0)
      }
      paste0(
        '<div style="padding: 4px; text-align: center; font-family: ',
        "'Helvetica Neue', sans-serif; color: #ffffff; background-color: ",
        node_col,
        '">tree value ',
        ddSci(frame[["node_value"]][[shown[[i]]]], 4L),
        "</div>",
        twocol2html(tables[[i]], value_col = c("#333333", taken))
      )
    },
    character(1L)
  )

  # Nodes and edges ----
  label <- vapply(
    shown,
    function(node) linad_node_rule(frame, node),
    character(1L)
  )
  if (show_node_value) {
    label <- paste0(
      label,
      "\n",
      ddSci(frame[["node_value"]][shown], 4L)
    )
  }
  if (show_ncases) {
    label <- paste0(label, "\n(n=", frame[["n"]][shown], ")")
  }
  colors <- rep(node_col, length(shown))
  colors[shown %in% terminal] <- leaf_col
  colors[is.na(frame[["parent"]][shown])] <- root_col
  nodes <- data.frame(
    id = shown,
    label = if (node_labels) label else NA_character_,
    value = frame[["n"]][shown],
    level = frame[["depth"]][shown] + 1L,
    title = tooltips,
    color = colors,
    shape = "box",
    shadow = FALSE,
    stringsAsFactors = FALSE
  )
  if (!node_labels) {
    nodes[["label"]] <- NULL
  }
  children <- setdiff(shown, shown[is.na(frame[["parent"]][shown])])
  edges <- data.frame(
    from = frame[["parent"]][children],
    to = children,
    value = frame[["n"]][children],
    stringsAsFactors = FALSE
  )

  if (verbosity > 0L) {
    msg(
      "Drawing LINAD tree with",
      length(terminal),
      ngettext(length(terminal), "leaf", "leaves"),
      "and",
      nrow(nodes),
      "nodes."
    )
  }

  # visNetwork ----
  plt <- visNetwork::visNetwork(
    nodes,
    edges,
    background = theme[["bg"]]
  )
  plt <- visNetwork::visHierarchicalLayout(
    plt,
    direction = direction,
    sortMethod = "directed"
  )
  plt <- visNetwork::visNodes(
    plt,
    font = list(color = theme[["fg"]], size = 18),
    borderWidth = 1
  )
  plt <- visNetwork::visEdges(
    plt,
    color = list(color = theme[["fg"]]),
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
    arrowStrikethrough = FALSE,
    hoverWidth = 0
  )
  # `tooltipStyle` has to clear the default padding and background, or
  # visNetwork's own tooltip chrome frames the table.
  visNetwork::visInteraction(
    plt,
    hover = TRUE,
    dragNodes = FALSE,
    dragView = TRUE,
    zoomView = TRUE,
    tooltipDelay = 50,
    tooltipStyle = paste(
      "position: fixed; visibility: hidden; padding: 0px;",
      "border: none; background-color: transparent;"
    )
  )
} # /rtemis::draw_linad
