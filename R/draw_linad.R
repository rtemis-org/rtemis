# draw_linad.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Node geometry ----
# vis.js's padding inside a box, in pixels. Generous on the horizontal, because
# a node holds two lines of different sizes and needs room to read as a card
# rather than as text with a rectangle around it.
LINAD_NODE_MARGIN_X <- 14L
LINAD_NODE_MARGIN_Y <- 9L

# Stroked centered on the box edge, so it contributes its full width across the
# two sides.
LINAD_NODE_BORDER <- 1L

# What the layout must reserve beyond the estimated text width. Derived rather
# than written down, so changing the margin cannot silently reintroduce
# overlaps.
LINAD_NODE_PADDING <- 2L * LINAD_NODE_MARGIN_X + LINAD_NODE_BORDER


# %% linad_palette ----
#' Node and table colors for a light or a dark theme
#'
#' Split by the background's perceived luminance rather than by theme name, so
#' any theme -- including one a user writes -- lands on a coherent set instead
#' of on whichever set happened to be hard-coded.
#'
#' Hue is reserved for a coefficient's sign. Everything here is therefore
#' achromatic except for the terminal-node fill.
#'
#' The table's border is the theme foreground, matching the hover marker: the
#' table is a mid-gray panel over mid-gray nodes and would otherwise have no
#' edge at all.
#'
#' @param theme `Theme` object.
#'
#' @return Named list of colors.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_palette <- function(theme) {
  if (col2grayscale(theme[["bg"]], "decimal") < 0.5) {
    list(
      root = "#7F7F7F",
      node = "#404040",
      leaf = rtemis_colors[["teal"]],
      table_bg = "#7F7F7F",
      header_bg = "#404040",
      table_fg = "#FFFFFF",
      empty = "#525252",
      border = theme[["fg"]]
    )
  } else {
    list(
      root = "#A8A8A8",
      node = "#D2D2D2",
      leaf = "#9FC0E0",
      table_bg = "#E6E6E6",
      header_bg = "#C6C6C6",
      table_fg = "#000000",
      empty = "#D2D2D2",
      border = theme[["fg"]]
    )
  }
} # /rtemis::linad_palette


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
  finite <- x[is.finite(x)]
  # `max()` of nothing is -Inf with a warning, and a tree whose nodes all fit
  # constants brings nothing here at all.
  extreme <- if (length(finite) > 0L) max(abs(finite)) else 0
  if (extreme == 0) {
    return(rep(gradient[[(n + 1L) %/% 2L]], length(x)))
  }
  # Anchoring the cut points at -extreme and +extreme is what puts zero in the
  # middle bin; cutting `x` alone would place the midpoint wherever the data
  # happened to sit.
  index <- cut(c(-extreme, x, extreme), n, labels = FALSE)
  gradient[index[seq.int(2L, length(x) + 1L)]]
} # /rtemis::linad_diverging


# %% linad_minus ----
#' Replace a leading hyphen-minus with the minus sign
#'
#' U+2212 is the width of a plus and matches the digits around it; a
#' hyphen-minus is narrower and sits low. The coefficient table already uses
#' it, so a node label that did not would disagree with the panel describing
#' it.
#'
#' @param x Character vector: Formatted numbers.
#'
#' @return Character vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_minus <- function(x) {
  sub("^-", "\u2212", x)
} # /rtemis::linad_minus


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
      paste0(feature, " \u2260 ", paste(levels_left, collapse = ", "))
    }
  } else {
    # The relational characters themselves, not their ASCII digraphs: ">=" is
    # two glyphs a reader assembles, U+2265 is the one they already know. This
    # needs no particular font, unlike a coding font's ligature.
    paste(
      feature,
      if (goes_left) "<" else "\u2265",
      linad_minus(ddSci(frame[["split_value"]][[parent]], 3L))
    )
  }
} # /rtemis::linad_node_rule


# %% linad_layout ----
#' Coordinates for a tree, guaranteeing no two nodes overlap
#'
#' vis.js's own hierarchical layout will not do this. Its `nodeSpacing` is
#' applied to a nominal node size rather than to the rendered box, so a node
#' whose label makes it wide overlaps its siblings regardless -- measured at 12
#' overlapping pairs on a 23-node tree, unchanged by turning the layout's
#' optimizations off and barely changed by raising `nodeSpacing` to 500.
#' Computing the coordinates here and handing vis.js fixed positions is what
#' makes overlap impossible rather than unlikely.
#'
#' Separation is **per pair**, from the two nodes' own widths, rather than one
#' spacing applied to every pair. A uniform spacing has to be as wide as the
#' widest node in the tree, which leaves every narrow node marooned in the gap
#' its widest sibling needed.
#'
#' Leaves are placed left to right in traversal order, each parent is centered
#' over its children, and then every level is swept once to push apart any pair
#' closer than the two half-widths plus `gap`. The sweep is what carries the
#' guarantee: centering a parent can pull it into a neighbor, so separation has
#' to be enforced after centering rather than assumed from it.
#'
#' @param frame data.table: The tree frame.
#' @param shown Integer vector: Node ids being drawn.
#' @param widths Numeric vector: Rendered width of each node, parallel to
#' `shown`.
#' @param gap Numeric: Clear space to leave between two boxes.
#' @param level_separation Numeric: Distance between depths.
#' @param direction Character: "UD", "DU", "LR" or "RL".
#'
#' @return List with numeric `x` and `y`, parallel to `shown`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_layout <- function(
  frame,
  shown,
  widths,
  gap,
  level_separation,
  direction = "UD"
) {
  half <- stats::setNames(widths / 2, as.character(shown))
  position <- stats::setNames(rep(NA_real_, length(shown)), as.character(shown))
  previous_leaf <- NULL
  children_of <- function(node) {
    kids <- c(frame[["left"]][[node]], frame[["right"]][[node]])
    kids[!is.na(kids) & kids %in% shown]
  }
  place <- function(node) {
    key <- as.character(node)
    kids <- children_of(node)
    if (length(kids) == 0L) {
      position[[key]] <<- if (is.null(previous_leaf)) {
        0
      } else {
        position[[previous_leaf]] + half[[previous_leaf]] + gap + half[[key]]
      }
      previous_leaf <<- key
      return(invisible(NULL))
    }
    for (kid in kids) {
      place(kid)
    }
    position[[key]] <<- mean(position[as.character(kids)])
    invisible(NULL)
  }
  place(shown[is.na(frame[["parent"]][shown])])

  depth <- frame[["depth"]][shown]
  across <- unname(position)
  halves <- unname(half)
  for (level in unique(depth)) {
    at_level <- which(depth == level)
    ordered <- at_level[order(across[at_level])]
    for (i in seq_along(ordered)[-1L]) {
      left <- ordered[[i - 1L]]
      right <- ordered[[i]]
      minimum <- across[[left]] + halves[[left]] + gap + halves[[right]]
      if (across[[right]] < minimum) {
        across[[right]] <- minimum
      }
    }
  }
  along <- depth * level_separation

  # vis.js measures y downwards, so "UD" is the identity.
  switch(
    direction,
    UD = list(x = across, y = along),
    DU = list(x = across, y = -along),
    LR = list(x = along, y = across),
    RL = list(x = -along, y = across)
  )
} # /rtemis::linad_layout


# %% linad_label_width ----
#' Estimated rendered width of a node box
#'
#' vis.js measures this in the browser; the layout has to know it in R. A
#' sans-serif glyph advances about 0.6 of the font size, and the label wraps at
#' `cap`, so the estimate is bounded above by the cap and cannot run away on a
#' long feature name.
#'
#' Deliberately generous. Underestimating a width would let two boxes touch,
#' which is the one outcome the layout exists to prevent; overestimating only
#' costs a little whitespace.
#'
#' @param labels Character vector: Node labels, newline-separated lines.
#' @param font_size Numeric: Font size in pixels.
#' @param cap Numeric: Largest width a label is allowed to reach.
#'
#' @return Numeric vector of widths.
#'
#' @author EDG
#' @keywords internal
#' @noRd
linad_label_width <- function(labels, font_size, cap) {
  # The markup that styles the eyebrow is not drawn, so measuring it would
  # reserve width for characters the reader never sees.
  plain <- gsub("</?[bi]>", "", labels)
  longest <- vapply(
    strsplit(plain, "\n", fixed = TRUE),
    function(lines) max(nchar(lines), 0L),
    integer(1L)
  )
  # Every line is measured at the base size although the eyebrow is set
  # smaller, which over-reserves. That is the safe direction: too little width
  # lets two boxes touch, too much costs whitespace.
  pmin(longest * font_size * 0.62, cap) + LINAD_NODE_PADDING
} # /rtemis::linad_label_width


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
#' @param top Optional Integer: Show this many coefficients per node, largest in
#' absolute value first. NULL shows every one, which on a wide model is neither
#' readable nor quick to render.
#' @param sort_coefs Logical: If TRUE, order each table by absolute coefficient.
#' @param direction Character \{"UD", "DU", "LR", "RL"\}: Tree direction.
#' @param node_width Integer: Largest node width in pixels. Labels wrap to stay
#' within it, which is what keeps a long feature name from widening a node past
#' the space the layout reserves for it.
#' @param node_gap Integer: Clear pixels to leave between two boxes on the same
#' level. Separation is computed per pair from the two nodes' own widths, so a
#' narrow node sits close to its neighbor instead of being spaced as though it
#' were the widest node in the tree.
#' @param level_separation Optional Integer: Pixels between levels.
#' @param font_size Integer: Label font size in pixels.
#' @param node_labels Logical: If TRUE, label nodes with their rule.
#' @param show_ncases Logical: If TRUE, include the case count in node labels.
#' @param show_node_value Logical: If TRUE, include the node's own value in its
#' label.
#' @param lo_col Character: Color for the most negative coefficient. Defaults
#' to the package-wide negative color, the same one `draw_volcano()` and
#' `plot_manhattan()` use.
#' @param mid_col Optional Character: Color at zero. NULL uses the coefficient
#' table's own background, so a coefficient of no consequence shows as no cell
#' at all.
#' @param hi_col Character: Color for the most positive coefficient. Defaults
#' to the package-wide positive color.
#' @param root_col Optional Character: Root node color. NULL follows the theme.
#' @param node_col Optional Character: Internal node color. NULL follows the
#' theme.
#' @param leaf_col Optional Character: Terminal node color. NULL follows the
#' theme. Kept off the two ends of the coefficient scale, so a hue in this plot
#' means a coefficient's sign and nothing else.
#' @param hover_col Optional Character: Border color marking the node the
#' coefficient table belongs to. NULL uses the theme foreground.
#' @param height Character: CSS height. A length rather than a percentage: a
#' percentage of a container with no height of its own collapses.
#' @param theme `Theme` object. Node fills, the coefficient table and the hover
#' marker all follow it, chosen by the background's luminance rather than by
#' theme name so that a custom theme is handled too. The two ends of the
#' coefficient scale do not follow it: both ends read on either background, and
#' a sign that changed color with the theme would be worse than one that did
#' not.
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
  top = 15L,
  sort_coefs = TRUE,
  direction = "UD",
  node_width = 180L,
  node_gap = 16L,
  level_separation = NULL,
  font_size = 14L,
  node_labels = TRUE,
  show_ncases = TRUE,
  show_node_value = TRUE,
  lo_col = SIGN_COLORS[["negative"]],
  mid_col = NULL,
  hi_col = SIGN_COLORS[["positive"]],
  root_col = NULL,
  node_col = NULL,
  leaf_col = NULL,
  hover_col = NULL,
  height = "600px",
  theme = choose_theme(getOption("rtemis_theme")),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("visNetwork")

  # Arguments ----
  check_is_S7(theme, Theme)
  direction <- match.arg(direction, c("UD", "DU", "LR", "RL"))
  palette <- linad_palette(theme)
  if (is.null(root_col)) {
    root_col <- palette[["root"]]
  }
  if (is.null(node_col)) {
    node_col <- palette[["node"]]
  }
  if (is.null(leaf_col)) {
    leaf_col <- palette[["leaf"]]
  }
  if (is.null(mid_col)) {
    # Zero is the table's own background, so a coefficient of no consequence
    # shows as no cell at all and color marks only what has magnitude.
    mid_col <- palette[["table_bg"]]
  }
  if (is.null(hover_col)) {
    hover_col <- theme[["fg"]]
  }
  if (is.null(level_separation)) {
    # Room for a wrapped rule plus the node value and the case count.
    level_separation <- 10L * font_size
  }
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
  # Column 1 is dropped rather than shown. It is not a fitted intercept -- the
  # node models are fitted without one, on a centered design -- but
  # `constant - center'slopes`, the algebraic cost of re-expressing the fit in
  # the uncentered form prediction needs. So it is the model's value at x = 0,
  # which for features living near 0.4 is a long extrapolation and reads as
  # nonsense beside an outcome it does not resemble. `node_value` is the level,
  # and it is what the caption carries.
  coefficients <- model@coefficients[shown, -1L, drop = FALSE]
  labels <- colnames(coefficients)
  tables <- lapply(seq_along(shown), function(i) {
    values <- coefficients[i, ]
    # A zero coefficient means this node's model did not select the feature, so
    # it says nothing and would otherwise pad the table with rows of 0.000 --
    # which is most of them under forward selection.
    index <- which(abs(values) > .Machine[["double.eps"]])
    index <- if (sort_coefs) {
      index[order(abs(values[index]), decreasing = TRUE)]
    } else {
      index
    }
    if (!is.null(top)) {
      index <- index[seq_len(min(length(index), top))]
    }
    data.frame(Term = labels[index], Coefficient = unname(values[index]))
  })
  # One gradient across every table: the same coefficient must be the same
  # color in every node, or the comparison the plot exists for is defeated
  # silently.
  slopes <- unlist(lapply(tables, function(tab) tab[["Coefficient"]]))
  slope_cols <- linad_diverging(slopes, lo_col, mid_col, hi_col)
  offsets <- cumsum(c(0L, vapply(tables, NROW, integer(1L))))
  tooltips <- vapply(
    seq_along(tables),
    function(i) {
      node_value <- ddSci(frame[["node_value"]][[shown[[i]]]], 4L)
      if (NROW(tables[[i]]) == 0L) {
        # No linear terms at all, which is what `node_model = "constant"` gives
        # everywhere and what a node too small to fit a model gives anywhere.
        return(twocol2html(
          data.frame(Term = "no linear terms", Coefficient = NA_real_),
          value_col = palette[["empty"]],
          caption = "Node value",
          caption_value = node_value,
          font_size = paste0(font_size, "px"),
          font_col = palette[["table_fg"]],
          header_bg = palette[["header_bg"]],
          table_bg = palette[["table_bg"]],
          border_col = palette[["border"]]
        ))
      }
      twocol2html(
        tables[[i]],
        value_col = slope_cols[seq.int(offsets[[i]] + 1L, offsets[[i + 1L]])],
        caption = "Node value",
        caption_value = node_value,
        font_size = paste0(font_size, "px"),
        font_col = palette[["table_fg"]],
        header_bg = palette[["header_bg"]],
        table_bg = palette[["table_bg"]],
        border_col = palette[["border"]]
      )
    },
    character(1L)
  )

  # Nodes and edges ----
  # Two lines, not four stacked numbers: an eyebrow carrying what the tree
  # predicts here and how many cases it holds, over the rule that defines the
  # node. `font.multi` styles the two tiers differently -- vis.js draws labels
  # to canvas, so this markup is the only typographic control there is, and a
  # rule's own `<` is safe because vis.js recognizes only its three tags.
  rule <- vapply(
    shown,
    function(node) linad_node_rule(frame, node),
    character(1L)
  )
  eyebrow <- character(length(shown))
  if (show_node_value) {
    eyebrow <- paste0(
      "<b>",
      linad_minus(ddSci(frame[["node_value"]][shown], 4L)),
      "</b>"
    )
  }
  if (show_ncases) {
    cases <- paste0("n=", frame[["n"]][shown])
    eyebrow <- if (show_node_value) {
      paste0(eyebrow, "<i>  \u00B7  ", cases, "</i>")
    } else {
      paste0("<i>", cases, "</i>")
    }
  }
  # A rule too wide for a node is broken after the feature name rather than
  # left to vis.js, which wraps on whatever space it reaches and so splits the
  # operator from the value it applies to. Every rule is `feature <op> value`
  # or `feature = levels`, so the first space is always the right break.
  too_wide <- nchar(rule) * font_size * 0.62 > node_width
  rule <- ifelse(too_wide, sub(" ", "\n", rule, fixed = TRUE), rule)
  label <- ifelse(nzchar(eyebrow), paste0(eyebrow, "\n", rule), rule)
  colors <- rep(node_col, length(shown))
  colors[shown %in% terminal] <- leaf_col
  colors[is.na(frame[["parent"]][shown])] <- root_col
  coordinates <- linad_layout(
    frame,
    shown,
    linad_label_width(label, font_size, node_width),
    node_gap,
    level_separation,
    direction
  )
  nodes <- data.frame(
    id = shown,
    label = if (node_labels) label else NA_character_,
    value = frame[["n"]][shown],
    x = coordinates[["x"]],
    y = coordinates[["y"]],
    title = tooltips,
    # Colors go in as an object per node rather than one string, because a
    # string makes vis.js derive the hover state from it and the hover border
    # could then never differ from the fill. The resting border matches the
    # background, so it is invisible until hover flips it to `hover_col` --
    # which is what ties the floating coefficient table to the node it
    # describes.
    color.background = colors,
    color.border = colors,
    color.hover.background = colors,
    color.hover.border = hover_col,
    color.highlight.background = colors,
    color.highlight.border = hover_col,
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
  # A fluid width makes the plot follow its pane. Without it visNetwork takes a
  # fixed pixel width and the plot ignores the space it is given -- a sizing
  # default, not a consequence of the positions being precomputed: those are
  # canvas-space, so refitting only ever changes zoom and pan. Height stays a
  # length rather than a percentage, since a percentage of a container with no
  # height of its own collapses to nothing.
  plt <- visNetwork::visNetwork(
    nodes,
    edges,
    width = "100%",
    height = height,
    background = theme[["bg"]]
  )
  # No `visHierarchicalLayout()`: the coordinates are already computed, and
  # vis.js's layout would recompute them into overlapping positions. Physics off
  # so nothing drifts from where it was placed.
  plt <- visNetwork::visPhysics(plt, enabled = FALSE)
  # Two tiers of type. `mod = ""` keeps the eyebrow upright: the tag selects a
  # style slot, it does not have to mean italic. The eyebrow is set against the
  # node fill with alpha rather than a fixed gray, so it recedes by the same
  # amount on the root, an internal node and a leaf, which are three different
  # colors.
  plt <- visNetwork::visNodes(
    plt,
    font = list(
      multi = "html",
      color = theme[["fg"]],
      size = font_size,
      bold = list(
        color = adjustcolor(theme[["fg"]], 0.85),
        size = font_size - 2L,
        mod = "bold"
      ),
      ital = list(
        color = adjustcolor(theme[["fg"]], 0.6),
        size = font_size - 2L,
        mod = ""
      )
    ),
    margin = list(
      top = LINAD_NODE_MARGIN_Y,
      bottom = LINAD_NODE_MARGIN_Y,
      left = LINAD_NODE_MARGIN_X,
      right = LINAD_NODE_MARGIN_X
    ),
    shapeProperties = list(borderRadius = 8L),
    widthConstraint = list(maximum = node_width),
    fixed = list(x = TRUE, y = TRUE),
    # vis.js changes the border *color* on hover but not its width -- only
    # selection reads `borderWidthSelected`. So the resting width is already
    # the hover width, and the border hides by matching the fill.
    borderWidth = LINAD_NODE_BORDER,
    borderWidthSelected = LINAD_NODE_BORDER
  )
  # Edge width carries the case count, but bounded: unbounded scaling makes the
  # root's edges into bands wide enough to obscure the nodes they connect.
  plt <- visNetwork::visEdges(
    plt,
    color = list(color = theme[["fg"]]),
    scaling = list(min = 1, max = 6),
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.4)),
    arrowStrikethrough = FALSE,
    smooth = FALSE,
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
