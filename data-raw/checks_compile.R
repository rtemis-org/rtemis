# checks_compile.R
# ::rtemis::
# 2026- EDG rtemis.org

# Compiles the rule set in `checks.R` from readable infix to JSONLogic, once,
# at generation time. The same source/artifact split the `prop_*` factories
# already have with JSON Schema: nothing consumes the infix but this compiler,
# and nothing hand-writes the JSONLogic.
#
# Three passes -- tokenize, parse, emit -- with a type check between the last
# two. What the type check is for is the point of the whole file, so it is
# stated here rather than at its definition:
#
# **JSONLogic's two variable semantics are loose equality and truthiness**, and
# a port is free to spell either differently. A divergence there produces a
# *wrong severity* rather than an error, which no test catches. Both are
# therefore unreachable from this language:
#
# - `==` and `!=` are not in the grammar. `===` and `!==` are the only equality
#   operators, so loose equality cannot be written, not merely discouraged.
# - Every conditional position -- an `if` condition, an operand of `and` / `or`
#   / `not`, a rule's `condition`, `applies_when` and `fix.when` -- must hold an
#   expression the type check *proves* boolean. A bare data reference infers as
#   `any`, so `if x then ...` is rejected and `if x !== null then ...` is not.
#
# The third rule is about scope rather than semantics. JSONLogic's `filter`,
# `map` and `reduce` *replace* the data with the current element:
#
#     scopedData.filter(function(datum) { return apply(scopedLogic, datum); })
#
# so nothing outside the array is reachable from inside the lambda. Every rule
# here that filters a profile array does so against a config-derived value --
# the outcome's name, whether missing values are skipped, which features are
# already removed -- and none of them can be written as a stock `filter`. So
# iteration belongs to the artifact's own frame (`scan`, and a rule's `over`),
# which extends the scope instead of replacing it, and the stock array
# operators survive only in aggregate forms whose lambda touches nothing
# outside the element. `check_lambda_closed()` enforces that.

# %% Tokenizer ----

# Operators, longest first so that `===` wins over `==` and `<=` over `<`.
CHECKS_OPERATORS <- c(
  "===",
  "!==",
  "<=",
  ">=",
  "==",
  "!=",
  "<",
  ">",
  "+",
  "-",
  "*",
  "/",
  "%",
  "(",
  ")",
  "[",
  "]",
  ",",
  "."
)

CHECKS_KEYWORDS <- c(
  "and",
  "or",
  "not",
  "in",
  "if",
  "then",
  "else",
  "true",
  "false",
  "null"
)


# %% checks_tokenize ----
# `src` Character(1): one infix expression.
# Returns a list of `list(type, value, pos)`; `type` is one of "number",
# "string", "name", "keyword", "op".
checks_tokenize <- function(src, where = "<expression>") {
  chars <- strsplit(src, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  i <- 1L
  out <- list()
  fail <- function(msg, at) {
    stop(
      "checks: ",
      msg,
      " at position ",
      at,
      " of ",
      where,
      ":\n  ",
      src,
      "\n  ",
      strrep(" ", at - 1L),
      "^",
      call. = FALSE
    )
  }
  while (i <= n) {
    ch <- chars[[i]]
    if (grepl("^[[:space:]]$", ch)) {
      i <- i + 1L
      next
    }
    # String literal, single or double quoted. No escapes: a rule's literals
    # are field names, dtypes and pointers, none of which contain a quote.
    if (ch %in% c("'", "\"")) {
      close <- which(chars[seq.int(i + 1L, n)] == ch)
      if (length(close) == 0L) {
        fail("unterminated string", i)
      }
      end <- i + close[[1L]]
      value <- paste0(chars[seq_len(end - i - 1L) + i], collapse = "")
      out[[length(out) + 1L]] <- list(
        type = "string",
        value = value,
        pos = i
      )
      i <- end + 1L
      next
    }
    if (grepl("^[0-9]$", ch)) {
      j <- i
      while (j <= n && grepl("^[0-9.]$", chars[[j]])) {
        j <- j + 1L
      }
      text <- paste0(chars[seq.int(i, j - 1L)], collapse = "")
      value <- suppressWarnings(as.numeric(text))
      if (is.na(value)) {
        fail(paste0("malformed number '", text, "'"), i)
      }
      out[[length(out) + 1L]] <- list(
        type = "number",
        value = value,
        pos = i
      )
      i <- j
      next
    }
    if (grepl("^[A-Za-z_]$", ch)) {
      j <- i
      while (j <= n && grepl("^[A-Za-z0-9_]$", chars[[j]])) {
        j <- j + 1L
      }
      text <- paste0(chars[seq.int(i, j - 1L)], collapse = "")
      out[[length(out) + 1L]] <- list(
        type = if (text %in% CHECKS_KEYWORDS) "keyword" else "name",
        value = text,
        pos = i
      )
      i <- j
      next
    }
    rest <- paste0(chars[seq.int(i, n)], collapse = "")
    op <- CHECKS_OPERATORS[startsWith(rest, CHECKS_OPERATORS)]
    if (length(op) == 0L) {
      fail(paste0("unexpected character '", ch, "'"), i)
    }
    op <- op[[which.max(nchar(op))]]
    # The ban, at the earliest point it can be stated. Naming the replacement
    # is what makes the message corrective rather than merely refusing.
    if (op %in% c("==", "!=")) {
      fail(
        paste0(
          "`",
          op,
          "` is loose equality, whose result varies between JSONLogic ports. ",
          "Write `",
          if (op == "==") "===" else "!==",
          "` instead"
        ),
        i
      )
    }
    out[[length(out) + 1L]] <- list(type = "op", value = op, pos = i)
    i <- i + nchar(op)
  }
  out
} # /checks_tokenize


# %% Parser ----

# Binary operator precedence, loosest first. `not` is prefix and binds tighter
# than `and`, so `not a and b` is `(not a) and b` -- the same grouping R gives
# `!a && b`, which is what an author transcribing a check will expect.
CHECKS_PRECEDENCE <- list(
  or = 1L,
  and = 2L,
  `===` = 4L,
  `!==` = 4L,
  `<` = 4L,
  `<=` = 4L,
  `>` = 4L,
  `>=` = 4L,
  `in` = 4L,
  `+` = 5L,
  `-` = 5L,
  `*` = 6L,
  `/` = 6L,
  `%` = 6L
)


# %% checks_parse ----
# `src` Character(1): one infix expression.
# Returns an AST node: a list with `kind` and kind-specific members.
checks_parse <- function(src, where = "<expression>") {
  tokens <- checks_tokenize(src, where)
  pos <- 1L

  fail <- function(msg) {
    stop("checks: ", msg, " in ", where, ":\n  ", src, call. = FALSE)
  }
  peek <- function() {
    if (pos > length(tokens)) NULL else tokens[[pos]]
  }
  take <- function() {
    tok <- peek()
    if (is.null(tok)) {
      fail("unexpected end of expression")
    }
    pos <<- pos + 1L
    tok
  }
  expect <- function(type, value) {
    tok <- take()
    if (!identical(tok[["type"]], type) || !identical(tok[["value"]], value)) {
      fail(paste0("expected `", value, "`, found `", tok[["value"]], "`"))
    }
    tok
  }
  at <- function(type, value) {
    tok <- peek()
    !is.null(tok) &&
      identical(tok[["type"]], type) &&
      identical(tok[["value"]], value)
  }

  parse_expr <- NULL

  parse_primary <- function() {
    tok <- take()
    node <- switch(
      tok[["type"]],
      number = list(kind = "number", value = tok[["value"]]),
      string = list(kind = "string", value = tok[["value"]]),
      keyword = switch(
        tok[["value"]],
        true = list(kind = "boolean", value = TRUE),
        false = list(kind = "boolean", value = FALSE),
        null = list(kind = "null"),
        `if` = {
          cond <- parse_expr(0L)
          expect("keyword", "then")
          yes <- parse_expr(0L)
          expect("keyword", "else")
          no <- parse_expr(0L)
          list(kind = "if", cond = cond, yes = yes, no = no)
        },
        not = list(kind = "not", operand = parse_expr(3L)),
        fail(paste0("`", tok[["value"]], "` cannot start an expression"))
      ),
      op = switch(
        tok[["value"]],
        `(` = {
          inner <- parse_expr(0L)
          expect("op", ")")
          inner
        },
        `[` = {
          elements <- list()
          if (!at("op", "]")) {
            repeat {
              elements[[length(elements) + 1L]] <- parse_expr(0L)
              if (!at("op", ",")) {
                break
              }
              take()
            }
          }
          expect("op", "]")
          list(kind = "array", elements = elements)
        },
        `-` = list(kind = "negate", operand = parse_expr(7L)),
        fail(paste0("`", tok[["value"]], "` cannot start an expression"))
      ),
      name = if (at("op", "(")) {
        take()
        args <- list()
        if (!at("op", ")")) {
          repeat {
            args[[length(args) + 1L]] <- parse_expr(0L)
            if (!at("op", ",")) {
              break
            }
            take()
          }
        }
        expect("op", ")")
        list(kind = "call", fn = tok[["value"]], args = args)
      } else {
        list(kind = "var", path = tok[["value"]])
      },
      fail(paste0("unexpected `", tok[["value"]], "`"))
    )
    # Dotted paths extend a reference and nothing else: `count(x).y` has no
    # meaning, and letting it parse would emit a `var` naming a path that
    # cannot exist.
    while (at("op", ".")) {
      take()
      field <- take()
      if (!field[["type"]] %in% c("name", "keyword")) {
        fail("expected a field name after `.`")
      }
      if (!identical(node[["kind"]], "var")) {
        fail("`.` may only follow a reference")
      }
      node[["path"]] <- paste0(node[["path"]], ".", field[["value"]])
    }
    node
  }

  parse_expr <- function(min_precedence) {
    left <- parse_primary()
    repeat {
      tok <- peek()
      if (is.null(tok) || !tok[["type"]] %in% c("op", "keyword")) {
        break
      }
      precedence <- CHECKS_PRECEDENCE[[tok[["value"]]]]
      if (is.null(precedence) || precedence < min_precedence) {
        break
      }
      take()
      right <- parse_expr(precedence + 1L)
      left <- list(
        kind = "binary",
        op = tok[["value"]],
        left = left,
        right = right
      )
    }
    left
  }

  node <- parse_expr(0L)
  if (pos <= length(tokens)) {
    fail(paste0("trailing `", tokens[[pos]][["value"]], "`"))
  }
  node
} # /checks_parse


# %% Functions ----
# The call vocabulary, with the JSONLogic each compiles to and the type each
# returns. Every one is either a scalar form or an aggregate over an array
# whose lambda reads only the element -- see the header on scope.
#
# `args` names the expected argument kinds: "expr" is any expression, "field"
# is a string literal naming a member of the array's elements. A field is a
# literal because it becomes part of a `var` path, which is resolved by name at
# evaluation time and so cannot be computed.
CHECKS_FUNCTIONS <- list(
  count = list(args = c("expr"), returns = "number"),
  sum = list(args = c("expr", "field"), returns = "number"),
  min = list(args = c("expr", "field"), returns = "number"),
  max = list(args = c("expr", "field"), returns = "number"),
  first = list(args = c("expr", "field"), returns = "any"),
  last = list(args = c("expr", "field"), returns = "any"),
  pluck = list(args = c("expr", "field"), returns = "array"),
  merge = list(args = c("expr", "expr"), returns = "array"),
  concat = list(args = NULL, returns = "string"),
  div_int = list(args = c("expr", "expr"), returns = "number"),
  round_half_even = list(args = c("expr"), returns = "number")
)


# The documents a rule set reads. A reference whose root is none of these and
# names no binding is a typo, and is rejected rather than emitted as a `var`
# that silently resolves to null at evaluation time.
CHECKS_INPUTS <- c("profile", "config", "outcome", "traits")

# Bound by the artifact's own iteration -- a `scan`'s source element or the
# element a rule runs `over`. Not an input, and only in scope where an
# iteration puts it there; `checks_compile_rules()` passes `item = TRUE` for
# exactly those positions.
CHECKS_ITEM <- "item"


# %% checks_infer ----
# The type of an expression, for the boolean-position rule.
#
# Five types, and the one that matters is `any`: a reference into an input
# document infers as `any`, and `any` is *not* boolean, so a raw reference in a
# conditional position is a compile error. That is the whole mechanism by which
# truthiness is unreachable.
#
# `node` AST node.
# `env`  Named list: binding name -> type, plus `.item` (Logical).
checks_infer <- function(node, env, where = "<expression>") {
  fail <- function(msg) {
    stop("checks: ", msg, " in ", where, call. = FALSE)
  }
  require_boolean <- function(child, what) {
    type <- checks_infer(child, env, where)
    if (!identical(type, "boolean")) {
      fail(paste0(
        what,
        " must be a boolean expression, but this one is `",
        type,
        "`. JSONLogic decides a non-boolean by truthiness, whose answer ",
        "varies between ports; compare explicitly instead (`x !== null`, ",
        "`x === true`, `count(x) > 0`)"
      ))
    }
    invisible(type)
  }
  switch(
    node[["kind"]],
    number = "number",
    string = "string",
    boolean = "boolean",
    null = "null",
    array = "array",
    var = {
      root <- strsplit(node[["path"]], ".", fixed = TRUE)[[1L]][[1L]]
      if (root %in% names(env) && !startsWith(root, ".")) {
        # A binding referenced whole carries its declared type; one reached
        # into is a member of a document and is `any` like any other.
        if (identical(root, node[["path"]])) env[[root]] else "any"
      } else if (root %in% CHECKS_INPUTS) {
        "any"
      } else if (identical(root, CHECKS_ITEM)) {
        if (!isTRUE(env[[".item"]])) {
          fail(
            "`item` is only in scope inside a `scan` or a rule's `over`"
          )
        }
        "any"
      } else {
        fail(paste0(
          "`",
          node[["path"]],
          "` names neither a binding nor an input (",
          paste(CHECKS_INPUTS, collapse = ", "),
          ")"
        ))
      }
    },
    not = {
      require_boolean(node[["operand"]], "the operand of `not`")
      "boolean"
    },
    negate = {
      checks_infer(node[["operand"]], env, where)
      "number"
    },
    `if` = {
      require_boolean(node[["cond"]], "an `if` condition")
      yes <- checks_infer(node[["yes"]], env, where)
      no <- checks_infer(node[["no"]], env, where)
      if (identical(yes, no)) {
        yes
      } else if (identical(yes, "null")) {
        no
      } else if (identical(no, "null")) {
        yes
      } else {
        "any"
      }
    },
    binary = {
      op <- node[["op"]]
      if (op %in% c("and", "or")) {
        require_boolean(node[["left"]], paste0("an operand of `", op, "`"))
        require_boolean(node[["right"]], paste0("an operand of `", op, "`"))
        return("boolean")
      }
      left <- checks_infer(node[["left"]], env, where)
      right <- checks_infer(node[["right"]], env, where)
      if (op %in% c("===", "!==", "<", "<=", ">", ">=", "in")) {
        if (op == "in" && !right %in% c("array", "any")) {
          fail(paste0(
            "the right operand of `in` must be an array, not `",
            right,
            "`"
          ))
        }
        return("boolean")
      }
      "number"
    },
    call = {
      spec <- CHECKS_FUNCTIONS[[node[["fn"]]]]
      if (is.null(spec)) {
        fail(paste0(
          "`",
          node[["fn"]],
          "()` is not one of the rule language's functions: ",
          paste(names(CHECKS_FUNCTIONS), collapse = ", ")
        ))
      }
      if (!is.null(spec[["args"]])) {
        if (length(node[["args"]]) != length(spec[["args"]])) {
          fail(paste0(
            "`",
            node[["fn"]],
            "()` takes ",
            length(spec[["args"]]),
            " argument",
            if (length(spec[["args"]]) == 1L) "" else "s",
            ", given ",
            length(node[["args"]])
          ))
        }
        for (i in seq_along(spec[["args"]])) {
          arg <- node[["args"]][[i]]
          if (
            identical(spec[["args"]][[i]], "field") &&
              !identical(arg[["kind"]], "string")
          ) {
            fail(paste0(
              "argument ",
              i,
              " of `",
              node[["fn"]],
              "()` names a field of the array's elements and must be a string ",
              "literal: the name becomes part of a `var` path and cannot be ",
              "computed"
            ))
          }
          checks_infer(arg, env, where)
        }
      } else {
        for (arg in node[["args"]]) {
          checks_infer(arg, env, where)
        }
      }
      spec[["returns"]]
    },
    fail(paste0("cannot infer the type of a `", node[["kind"]], "` node"))
  )
} # /checks_infer


# %% Emitter ----

# %% .var ----
# A `var` node, with a binding reference namespaced so that a binding and an
# input document can never collide at evaluation time.
.var <- function(path, bindings) {
  root <- strsplit(path, ".", fixed = TRUE)[[1L]][[1L]]
  list(var = if (root %in% bindings) paste0("bindings.", path) else path)
} # /.var


# %% .reduce ----
# A stock `reduce` whose lambda reads only `current` and `accumulator`. Every
# aggregate is built here rather than authored, which is what keeps the lambda
# closed over the element -- see the header on scope.
.reduce <- function(array_logic, step, initial) {
  list(reduce = list(array_logic, step, initial))
} # /.reduce


# %% checks_emit ----
# `node`     AST node.
# `bindings` Character: names that resolve to the `bindings` namespace.
checks_emit <- function(node, bindings = character()) {
  emit <- function(x) checks_emit(x, bindings)
  # A conditional position never holds a bare reference. The type check has
  # already proved the expression boolean, so JSONLogic's truthiness would give
  # the right answer -- but a port author reading the artifact has no access to
  # that proof, and `{"===": [x, true]}` needs none. Everything else in a
  # conditional position is already a comparison and is emitted unchanged.
  emit_condition <- function(x) {
    if (identical(x[["kind"]], "var")) {
      list(`===` = list(emit(x), TRUE))
    } else {
      emit(x)
    }
  }
  acc <- list(var = "accumulator")
  field <- function(name) list(var = paste0("current.", name))
  switch(
    node[["kind"]],
    number = node[["value"]],
    string = node[["value"]],
    boolean = node[["value"]],
    null = NULL,
    array = lapply(node[["elements"]], emit),
    var = .var(node[["path"]], bindings),
    not = list(`!` = list(emit_condition(node[["operand"]]))),
    negate = list(`-` = list(emit(node[["operand"]]))),
    `if` = list(
      `if` = list(
        emit_condition(node[["cond"]]),
        emit(node[["yes"]]),
        emit(node[["no"]])
      )
    ),
    binary = {
      op <- node[["op"]]
      conditional <- op %in% c("and", "or")
      left <- if (conditional) {
        emit_condition(node[["left"]])
      } else {
        emit(node[["left"]])
      }
      right <- if (conditional) {
        emit_condition(node[["right"]])
      } else {
        emit(node[["right"]])
      }
      # `a and b and c` parses left-associatively; flattening it into one
      # n-ary node is what keeps the published document readable at the
      # nesting depth these guards reach.
      operands <- if (
        op %in%
          c("and", "or") &&
          identical(node[["left"]][["kind"]], "binary") &&
          identical(node[["left"]][["op"]], op)
      ) {
        c(left[[op]], list(right))
      } else {
        list(left, right)
      }
      stats::setNames(list(operands), op)
    },
    call = {
      fn <- node[["fn"]]
      args <- node[["args"]]
      switch(
        fn,
        count = .reduce(emit(args[[1L]]), list(`+` = list(acc, 1)), 0),
        sum = .reduce(
          emit(args[[1L]]),
          list(`+` = list(acc, field(args[[2L]][["value"]]))),
          0
        ),
        min = ,
        max = .reduce(
          emit(args[[1L]]),
          list(
            `if` = list(
              list(`===` = list(acc, NULL)),
              field(args[[2L]][["value"]]),
              stats::setNames(
                list(list(acc, field(args[[2L]][["value"]]))),
                fn
              )
            )
          ),
          NULL
        ),
        # `first` keeps the earliest element, which is what `which.min()` does
        # on a tie; a plain `reduce` returning `current` would keep the last
        # and disagree with the reference implementation exactly there.
        first = .reduce(
          emit(args[[1L]]),
          list(
            `if` = list(
              list(`===` = list(acc, NULL)),
              field(args[[2L]][["value"]]),
              acc
            )
          ),
          NULL
        ),
        last = .reduce(
          emit(args[[1L]]),
          field(args[[2L]][["value"]]),
          NULL
        ),
        pluck = list(
          map = list(
            emit(args[[1L]]),
            list(var = args[[2L]][["value"]])
          )
        ),
        merge = list(merge = list(emit(args[[1L]]), emit(args[[2L]]))),
        concat = list(cat = lapply(args, emit)),
        div_int = {
          a <- emit(args[[1L]])
          b <- emit(args[[2L]])
          list(`/` = list(list(`-` = list(a, list(`%` = list(a, b)))), b))
        },
        # R's `round()` is round-half-to-even, and the tie rule is
        # load-bearing: at n_rows 5 and train_p 0.9 it gives 4 test cases
        # rather than 5, which is a warning rather than an error. JSONLogic
        # has no rounding operator, so it is spelled out. Exact for the
        # non-negative magnitudes here: `x %% 1` is a remainder, which is
        # exact, so the `=== 0.5` test is true precisely at a tie.
        round_half_even = {
          x <- emit(args[[1L]])
          frac <- list(`%` = list(x, 1))
          floor_x <- list(`-` = list(x, frac))
          list(
            `+` = list(
              floor_x,
              list(
                `if` = list(
                  list(
                    or = list(
                      list(`>` = list(frac, 0.5)),
                      list(
                        and = list(
                          list(`===` = list(frac, 0.5)),
                          list(`===` = list(list(`%` = list(floor_x, 2)), 1))
                        )
                      )
                    )
                  ),
                  1,
                  0
                )
              )
            )
          )
        },
        stop("checks: no emitter for `", fn, "()`", call. = FALSE)
      )
    },
    stop("checks: no emitter for a `", node[["kind"]], "` node", call. = FALSE)
  )
} # /checks_emit


# %% checks_compile ----
# Parse, type-check and emit one infix expression.
#
# `src`   Character(1): the expression.
# `env`   Named list: binding name -> type, plus `.item` (Logical).
# `where` Character(1): what to name in an error message.
checks_compile <- function(src, env, where = "<expression>") {
  node <- checks_parse(src, where)
  type <- checks_infer(node, env, where)
  list(logic = checks_emit(node, setdiff(names(env), ".item")), type = type)
} # /checks_compile


# %% checks_compile_boolean ----
# As `checks_compile()`, refusing anything the type check does not prove
# boolean. Every guard position goes through here.
checks_compile_boolean <- function(src, env, where) {
  # A guard that is a single binding reference is emitted as `x === true`, for
  # the same reason `emit_condition()` does it inside an expression: the
  # artifact should not need the type check to be read as truthiness-free. Only
  # where the reference is already boolean -- a number-valued guard is a
  # mistake, and comparing it to `true` would hide one.
  node <- checks_parse(src, where)
  if (
    identical(node[["kind"]], "var") &&
      identical(checks_infer(node, env, where), "boolean")
  ) {
    src <- paste0(src, " === true")
  }
  out <- checks_compile(src, env, where)
  if (!identical(out[["type"]], "boolean")) {
    stop(
      "checks: ",
      where,
      " must be a boolean expression, but this one is `",
      out[["type"]],
      "`:\n  ",
      src,
      call. = FALSE
    )
  }
  out[["logic"]]
} # /checks_compile_boolean


# %% Authoring forms ----
# The five constructors `checks.R` is written with. Each is a plain list; the
# compiling is `checks_build()`'s job, so the rule set stays readable as data.

# %% expr ----
# A binding: one expression, evaluated once per validation, in declaration
# order. `type` is asserted against what the compiler infers, so a binding that
# stops being boolean is caught where it is declared rather than at the guard
# that reads it.
expr <- function(name, src, type) {
  list(kind = "expr", name = name, src = src, type = type)
} # /expr


# %% scan ----
# A binding over a collection: the artifact's own iteration.
#
# Not JSONLogic's `filter`. A stock `filter` replaces the data with the current
# element, so `where` could not reach the outcome's name or any other binding;
# a `scan` evaluates `where` and `select` with the element bound to `item` and
# everything else still in scope.
#
# `select` is also the only way to build a record, JSONLogic having no object
# constructor. Given a name -> expression map, each element becomes an object
# with exactly those members; absent, elements pass through unchanged.
scan <- function(name, over, where = NULL, select = NULL) {
  list(kind = "scan", name = name, over = over, where = where, select = select)
} # /scan


# %% scan_pointers ----
# A scan whose source is a set of JSON Pointers into the config, resolved per
# RFC 6901. This is what turns config *object keys* -- which no expression
# language can iterate -- into an array of records.
#
# Each element is `{pointer, name, value}`: the pointer as written, its last
# segment, and what it resolves to, or null where it resolves to nothing.
scan_pointers <- function(name, pointers, where = NULL, select = NULL) {
  list(
    kind = "scan",
    name = name,
    pointers = pointers,
    where = where,
    select = select
  )
} # /scan_pointers


# %% lit ----
# A patch value that is a JSON literal rather than an expression. Needed for
# the one place a patch creates an empty object for a later operation to fill.
lit <- function(value) {
  structure(list(value = value), class = "checks_literal")
} # /lit


# %% op ----
# One RFC 6902 patch operation. `path` is a template resolved against the
# finding's evidence, `value` an expression or a `lit()`.
op <- function(op, path, value) {
  list(op = op, path = path, value = value)
} # /op


# %% fix ----
# The patch a finding carries, and the condition under which it is offered at
# all -- a repair that cannot be derived is better absent than guessed.
fix <- function(patch, when = NULL) {
  list(when = when, patch = patch)
} # /fix


# %% rule ----
rule <- function(
  id,
  code,
  condition,
  severity,
  evidence,
  message,
  applies_when = NULL,
  over = NULL,
  let = NULL,
  slots = NULL,
  fix = NULL
) {
  list(
    id = id,
    code = code,
    over = over,
    let = let,
    applies_when = applies_when,
    condition = condition,
    severity = severity,
    evidence = evidence,
    slots = slots,
    message = message,
    fix = fix
  )
} # /rule


# %% MESSAGE_SLOT ----
# `{key}`, or `{key|singular|plural}`. A slot names an evidence key and nothing
# else: no expressions, so a renderer substitutes rather than evaluates.
MESSAGE_SLOT <- "\\{([A-Za-z_][A-Za-z0-9_]*)(\\|[^{}|]*\\|[^{}|]*)?\\}"


# %% message_slots ----
# The evidence keys a template reads.
message_slots <- function(template) {
  matches <- gregexpr(MESSAGE_SLOT, template)[[1L]]
  if (matches[[1L]] == -1L) {
    return(character())
  }
  found <- regmatches(template, gregexpr(MESSAGE_SLOT, template))[[1L]]
  sub("^\\{([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", found)
} # /message_slots


# %% checks_build_let ----
# Compile the bindings, threading the type environment forward so that each may
# reference those declared before it and none may reference itself.
checks_build_let <- function(let) {
  env <- list()
  out <- list()
  for (entry in let) {
    name <- entry[["name"]]
    if (name %in% names(env)) {
      stop("checks: binding `", name, "` is declared twice", call. = FALSE)
    }
    where <- paste0("binding `", name, "`")
    if (identical(entry[["kind"]], "expr")) {
      compiled <- checks_compile(entry[["src"]], env, where)
      if (
        !identical(compiled[["type"]], entry[["type"]]) &&
          !identical(entry[["type"]], "any")
      ) {
        stop(
          "checks: ",
          where,
          " is declared `",
          entry[["type"]],
          "` but compiles to `",
          compiled[["type"]],
          "`",
          call. = FALSE
        )
      }
      out[[length(out) + 1L]] <- list(
        name = name,
        kind = "expr",
        logic = compiled[["logic"]]
      )
      env[[name]] <- entry[["type"]]
      next
    }
    # A scan. `where` and `select` see the element as `item`; the source does
    # not, being what produces the elements.
    item_env <- c(env, list(.item = TRUE))
    node <- list(name = name, kind = "scan")
    if (!is.null(entry[["pointers"]])) {
      node[["source"]] <- list(pointers = as.list(entry[["pointers"]]))
    } else {
      source <- checks_compile(entry[["over"]], env, paste0(where, " source"))
      node[["source"]] <- list(logic = source[["logic"]])
    }
    if (!is.null(entry[["where"]])) {
      node[["where"]] <- checks_compile_boolean(
        entry[["where"]],
        item_env,
        paste0(where, " `where`")
      )
    }
    if (!is.null(entry[["select"]])) {
      node[["select"]] <- lapply(
        names(entry[["select"]]),
        function(field) {
          checks_compile(
            entry[["select"]][[field]],
            item_env,
            paste0(where, " `select` member `", field, "`")
          )[["logic"]]
        }
      )
      names(node[["select"]]) <- names(entry[["select"]])
    }
    out[[length(out) + 1L]] <- node
    env[[name]] <- "array"
  }
  list(let = out, env = env)
} # /checks_build_let


# %% checks_build_rules ----
checks_build_rules <- function(rules, env) {
  ids <- vapply(rules, `[[`, character(1L), "id")
  duplicated_ids <- unique(ids[duplicated(ids)])
  if (length(duplicated_ids) > 0L) {
    stop(
      "checks: duplicate rule id: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }
  lapply(rules, function(r) {
    where <- paste0("rule `", r[["id"]], "`")
    item_env <- if (is.null(r[["over"]])) {
      env
    } else {
      if (!identical(env[[r[["over"]]]], "array")) {
        stop(
          "checks: ",
          where,
          " runs `over` `",
          r[["over"]],
          "`, which is not a scan",
          call. = FALSE
        )
      }
      c(env, list(.item = TRUE))
    }
    out <- list(id = r[["id"]], code = r[["code"]])
    if (!is.null(r[["over"]])) {
      out[["over"]] <- r[["over"]]
    }
    # Rule-local bindings. A quantity derived from the element a rule runs
    # `over` cannot be a global binding -- there is no `item` there -- and
    # without these it would be written out again in the condition, in the
    # evidence and in the message. Three copies that must agree is the drift
    # this whole exercise is avoiding.
    if (!is.null(r[["let"]])) {
      shadowed <- intersect(names(r[["let"]]), names(env))
      if (length(shadowed) > 0L) {
        stop(
          "checks: ",
          where,
          " declares local binding",
          if (length(shadowed) == 1L) " " else "s ",
          paste0("`", shadowed, "`", collapse = ", "),
          ", which shadow",
          if (length(shadowed) == 1L) "s" else "",
          " a binding of the same name",
          call. = FALSE
        )
      }
      out[["let"]] <- list()
      for (local in names(r[["let"]])) {
        compiled <- checks_compile(
          r[["let"]][[local]],
          item_env,
          paste0(where, " local binding `", local, "`")
        )
        out[["let"]][[local]] <- compiled[["logic"]]
        item_env[[local]] <- compiled[["type"]]
      }
    }
    if (!is.null(r[["applies_when"]])) {
      out[["applies_when"]] <- checks_compile_boolean(
        r[["applies_when"]],
        item_env,
        paste0(where, " `applies_when`")
      )
    }
    out[["condition"]] <- checks_compile_boolean(
      r[["condition"]],
      item_env,
      paste0(where, " `condition`")
    )
    out[["severity"]] <- r[["severity"]]
    out[["evidence"]] <- lapply(names(r[["evidence"]]), function(key) {
      checks_compile(
        r[["evidence"]][[key]],
        item_env,
        paste0(where, " evidence `", key, "`")
      )[["logic"]]
    })
    names(out[["evidence"]]) <- names(r[["evidence"]])
    # Presentation values a message needs and evidence should not carry: a
    # quoted list of column names is not a measured fact. Keeping them apart is
    # what lets `evidence` stay exactly what the reference implementation
    # reports.
    if (!is.null(r[["slots"]])) {
      collide <- intersect(names(r[["slots"]]), names(r[["evidence"]]))
      if (length(collide) > 0L) {
        stop(
          "checks: ",
          where,
          " declares ",
          paste0("`", collide, "`", collapse = ", "),
          " as both evidence and a slot",
          call. = FALSE
        )
      }
      out[["slots"]] <- lapply(names(r[["slots"]]), function(key) {
        checks_compile(
          r[["slots"]][[key]],
          item_env,
          paste0(where, " slot `", key, "`")
        )[["logic"]]
      })
      names(out[["slots"]]) <- names(r[["slots"]])
    }
    out[["message"]] <- r[["message"]]
    if (!is.null(r[["fix"]])) {
      f <- r[["fix"]]
      node <- list()
      if (!is.null(f[["when"]])) {
        node[["when"]] <- checks_compile_boolean(
          f[["when"]],
          item_env,
          paste0(where, " `fix.when`")
        )
      }
      node[["patch"]] <- lapply(f[["patch"]], function(operation) {
        value <- operation[["value"]]
        list(
          op = operation[["op"]],
          path = operation[["path"]],
          value = if (inherits(value, "checks_literal")) {
            value[["value"]]
          } else {
            checks_compile(
              value,
              item_env,
              paste0(where, " patch value")
            )[["logic"]]
          }
        )
      })
      out[["fix"]] <- node
    }
    out
  })
} # /checks_build_rules
