#' Decimal fraction integer linter
#'
#' Check for use of `as.integer(x * <float>)` and suggest an integer arithmetic
#' equivalent like `as.integer((x * 1461L)%/%4L)`.
#'
#' @examples
#' # will produce a lint
#' lint(
#'   text = "as.integer(x * 365.25)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(365.25 * x)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(x * 0.1)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' lint(
#'   text = "as.integer(x / 0.13)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "as.integer(x * 365)",
#'   linters = decimal_fraction_linter()
#' )
#'
#' @evalRd rd_tags("decimal_fraction_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
decimal_fraction_linter <- function() {
  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("as.integer")
    if (length(xml_calls) == 0L) {
      return(list())
    }

    # Match as.integer calls where the argument is a simple binary expr with a
    # decimal literal (including 1e-3-style) so we can safely suggest integer
    # arithmetic replacements.
    bad_expr <- xml_find_all(
      xml_calls,
      "parent::expr[
        expr[2][
          (
            OP-STAR
            and expr//NUM_CONST
          ) or (
            OP-SLASH
            and expr[2]//NUM_CONST
          )
        ]
        and expr[2]//NUM_CONST[
          contains(text(), '.')
          or contains(translate(text(), 'E', 'e'), 'e-')
        ]
      ]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    bad_expr <- strip_comments_from_subtree(bad_expr)
    bad_expr <- xml_find_all(
      bad_expr,
      "self::expr[expr[2][count(expr) = 2]]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    arg_expr <- xml_find_all(bad_expr, "expr[2]")
    expr_children <- xml_find_all(arg_expr, "./expr")
    is_numeric_child <- vapply(expr_children, function(expr) {
      !is.na(xml_find_first(expr, ".//NUM_CONST"))
    }, logical(1L))
    num_expr <- xml_find_all(expr_children[is_numeric_child], ".//NUM_CONST")
    num_text <- xml_text(num_expr)
    other_expr <- expr_children[!is_numeric_child]
    other_factor <- xml_text(other_expr)
    operator <- vapply(arg_expr, function(expr) {
      xml_name(xml_find_first(expr, "./OP-STAR | ./OP-SLASH"))
    }, character(1L))
    # Unary minus is its own node in the XML AST, so we need to inspect ancestors
    # to preserve sign in the replacement suggestion.
    is_negative <- vapply(num_expr, function(expr) {
      !is.na(xml_find_first(expr, "ancestor::expr[OP-MINUS and count(expr) = 1]"))
    }, logical(1L))

    ok <- nzchar(other_factor)
    bad_expr <- bad_expr[ok]
    arg_expr <- arg_expr[ok]
    num_text <- num_text[ok]
    other_factor <- other_factor[ok]
    operator <- operator[ok]
    is_negative <- is_negative[ok]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    fractions <- lapply(num_text, float_to_fraction)
    has_fraction <- vapply(fractions, Negate(is.null), logical(1L))
    bad_expr <- bad_expr[has_fraction]
    arg_expr <- arg_expr[has_fraction]
    other_factor <- other_factor[has_fraction]
    fractions <- fractions[has_fraction]
    operator <- operator[has_fraction]
    is_negative <- is_negative[has_fraction]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    arg_text <- xml_text(arg_expr)
    lint_message <- vapply(seq_along(bad_expr), function(idx) {
      fraction <- fractions[[idx]]
      sign_factor <- ifelse(is_negative[[idx]], -1L, 1L)
      numerator <- sign_factor * fraction[["numerator"]]
      denominator <- fraction[["denominator"]]
      if (operator[[idx]] == "OP-SLASH") {
        numerator <- sign_factor * fraction[["denominator"]]
        denominator <- fraction[["numerator"]]
      }
      mult_part <- if (numerator == 1L) "" else sprintf(" * %dL", numerator)
      div_part <- if (denominator == 1L) "" else sprintf(" %%/%% %dL", denominator)
      replacement <- sprintf(
        "as.integer((%s)%s%s)",
        other_factor[[idx]],
        mult_part,
        div_part
      )
      sprintf(
        "Use %s instead of as.integer(%s) to avoid floating-point rounding.",
        replacement,
        arg_text[[idx]]
      )
    }, character(1L))

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning",
      column_number_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_start_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_end_xpath = "number(./expr[2]//NUM_CONST/@col2)"
    )
  })
}

float_to_fraction <- function(number) {
  split_exp <- strsplit(number, "[eE]", perl = TRUE)[[1L]]
  mantissa <- split_exp[[1L]]
  exponent <- if (length(split_exp) > 1L) as.integer(split_exp[[2L]]) else 0L
  if (skip_float_fraction(mantissa, exponent)) {
    return(NULL)
  }

  parts <- strsplit(mantissa, ".", fixed = TRUE)[[1L]]
  integer_part <- parts[[1L]]
  fractional_part <- if (length(parts) > 1L) parts[[2L]] else ""
  integer_part <- ifelse(nzchar(integer_part), integer_part, "0")

  # Use decimal digits for numerator/denominator to avoid floating-point drift.
  numerator <- as.integer(paste0(integer_part, fractional_part))
  if (is.na(numerator)) {
    return(NULL)
  }
  denominator <- 10.0^nchar(fractional_part)
  if (exponent < 0L) {
    denominator <- denominator * 10.0^abs(exponent)
  }
  normalize_fraction(numerator, denominator)
}

skip_float_fraction <- function(mantissa, exponent) {
  if (!grepl(".", mantissa, fixed = TRUE)) {
    return(exponent >= 0L)
  }
  # Positive exponents create integer-like values (e.g., 3.2e2) that we skip.
  if (exponent > 0L) {
    return(TRUE)
  }
  if (exponent >= 0L && !grepl("[1-9]", mantissa)) {
    return(TRUE)
  }
  FALSE
}

normalize_fraction <- function(numerator, denominator) {
  if (abs(numerator) > .Machine[["integer.max"]] || denominator > .Machine[["integer.max"]]) {
    return(NULL)
  }
  numerator <- as.integer(numerator)
  denominator <- as.integer(denominator)
  if (denominator == 0L) {
    return(NULL)
  }
  divisor <- int_gcd(abs(numerator), denominator)
  numerator <- numerator %/% divisor
  denominator <- denominator %/% divisor
  if (denominator == 1L) {
    return(NULL)
  }

  list(numerator = numerator, denominator = denominator)
}

int_gcd <- function(a, b) {
  while (b != 0L) {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  abs(a)
}
