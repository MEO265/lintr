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
      "parent::expr[
        expr[2][
          OP-STAR
          and expr/NUM_CONST
          and expr/NUM_CONST[
            contains(text(), '.')
            and not(contains(translate(text(), 'E', 'e'), 'e'))
          ]
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
    num_expr <- xml_find_all(arg_expr, "./expr/NUM_CONST")
    num_text <- xml_text(num_expr)
    other_expr <- xml_find_all(arg_expr, "./expr[not(NUM_CONST)]")
    other_factor <- xml_text(other_expr)

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

float_to_fraction <- function(number) {
  if (!grepl("\\.\\d", number)) {
    return(NULL)
  }
  parts <- strsplit(number, ".", fixed = TRUE)[[1L]]
  integer_part <- parts[[1L]]
  fractional_part <- if (length(parts) > 1L) parts[[2L]] else ""
  integer_part <- ifelse(nzchar(integer_part), integer_part, "0")

  numerator <- as.integer(paste0(integer_part, fractional_part))
  if (is.na(numerator)) {
    return(NULL)
  }
  denominator <- 10.0^nchar(fractional_part)

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
