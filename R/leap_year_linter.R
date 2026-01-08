#' Leap year integer linter
#'
#' Check for use of `as.integer(x * <float>)` and suggest an integer arithmetic
#' equivalent like `as.integer((x * 1461L)%/%4L)`.
#'
#' @examples
#' # will produce a lint
#' lint(
#'   text = "as.integer(x * 365.25)",
#'   linters = leap_year_linter()
#' )
#'
#' lint(
#'   text = "as.integer(365.25 * x)",
#'   linters = leap_year_linter()
#' )
#'
#' lint(
#'   text = "as.integer(x * 0.1)",
#'   linters = leap_year_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "as.integer(x * 365)",
#'   linters = leap_year_linter()
#' )
#'
#' @evalRd rd_tags("leap_year_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
leap_year_linter <- function() {
  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("as.integer")
    if (length(xml_calls) == 0L) {
      return(list())
    }

    bad_expr <- xml_find_all(
      xml_calls,
      "parent::expr[expr[2][OP-STAR and expr/NUM_CONST]]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    bad_expr <- strip_comments_from_subtree(bad_expr)
    arg_expr <- xml_find_all(bad_expr, "expr[2]")
    is_simple_multiply <- vapply(arg_expr, function(arg) {
      child_nodes <- xml_children(arg)
      child_nodes <- child_nodes[xml_name(child_nodes) != "COMMENT"]
      identical(xml_name(child_nodes), c("expr", "OP-STAR", "expr"))
    }, logical(1L))
    bad_expr <- bad_expr[is_simple_multiply]
    arg_expr <- arg_expr[is_simple_multiply]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    num_expr <- xml_find_all(arg_expr, "./expr/NUM_CONST")
    num_text <- xml_text(num_expr)
    is_float <- vapply(num_text, is_float_literal, logical(1L))
    num_expr <- num_expr[is_float]
    num_text <- num_text[is_float]
    arg_expr <- arg_expr[is_float]
    bad_expr <- bad_expr[is_float]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    other_factor <- vapply(arg_expr, function(arg) {
      child_expr <- xml_find_all(arg, "./expr")
      is_number <- vapply(child_expr, function(child) {
        !is.na(xml_find_first(child, "./NUM_CONST"))
      }, logical(1L))
      other_child <- child_expr[!is_number]
      if (length(other_child) == 0L) {
        return("")
      }
      xml_text(other_child[[1L]])
    }, character(1L))

    ok <- nzchar(other_factor)
    bad_expr <- bad_expr[ok]
    arg_expr <- arg_expr[ok]
    num_text <- num_text[ok]
    other_factor <- other_factor[ok]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    fractions <- lapply(num_text, float_to_fraction)
    has_fraction <- vapply(fractions, Negate(is.null), logical(1L))
    bad_expr <- bad_expr[has_fraction]
    arg_expr <- arg_expr[has_fraction]
    other_factor <- other_factor[has_fraction]
    fractions <- fractions[has_fraction]
    if (length(bad_expr) == 0L) {
      return(list())
    }

    arg_text <- xml_text(arg_expr)
    lint_message <- vapply(seq_along(bad_expr), function(idx) {
      fraction <- fractions[[idx]]
      replacement <- sprintf(
        "as.integer(((%s) * %dL)%%/%% %dL)",
        other_factor[[idx]],
        fraction[["numerator"]],
        fraction[["denominator"]]
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
      type = "warning"
    )
  })
}

is_float_literal <- function(number) {
  grepl("[.eE]", number)
}

float_to_fraction <- function(number) {
  if (!is_float_literal(number)) {
    return(NULL)
  }
  split_exp <- strsplit(number, "[eE]", perl = TRUE)[[1L]]
  mantissa <- split_exp[[1L]]
  exponent <- if (length(split_exp) > 1L) as.integer(split_exp[[2L]]) else 0L

  mantissa_fraction <- decimal_to_fraction(mantissa)
  if (is.null(mantissa_fraction)) {
    return(NULL)
  }

  fraction <- scale_fraction(mantissa_fraction, exponent)
  if (is.null(fraction)) {
    return(NULL)
  }
  normalize_fraction(fraction)
}

decimal_to_fraction <- function(number) {
  if (!nzchar(number)) {
    return(NULL)
  }
  if (!grepl(".", number, fixed = TRUE)) {
    numerator <- as.integer(number)
    return(list(numerator = numerator, denominator = 1L))
  }

  parts <- strsplit(number, ".", fixed = TRUE)[[1L]]
  integer_part <- parts[[1L]]
  fractional_part <- parts[[2L]]
  if (!nzchar(integer_part)) {
    integer_part <- "0"
  }
  if (!nzchar(fractional_part)) {
    numerator <- as.integer(integer_part)
    return(list(numerator = numerator, denominator = 1L))
  }

  numerator <- as.integer(paste0(integer_part, fractional_part))
  denominator <- 10.0^nchar(fractional_part)
  divisor <- int_gcd(abs(numerator), denominator)
  numerator <- numerator %/% divisor
  denominator <- as.integer(denominator %/% divisor)
  list(numerator = numerator, denominator = denominator)
}

scale_fraction <- function(fraction, exponent) {
  numerator <- fraction[["numerator"]]
  denominator <- fraction[["denominator"]]
  if (exponent > 0L) {
    numerator <- numerator * 10.0^exponent
  } else if (exponent < 0L) {
    denominator <- denominator * 10.0^abs(exponent)
  }

  list(numerator = numerator, denominator = denominator)
}

normalize_fraction <- function(fraction) {
  numerator <- fraction[["numerator"]]
  denominator <- fraction[["denominator"]]
  if (abs(numerator) > .Machine[["integer.max"]] || denominator > .Machine[["integer.max"]]) {
    return(NULL)
  }
  denominator <- as.integer(denominator)
  numerator <- as.integer(numerator)
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
