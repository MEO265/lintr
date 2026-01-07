#' Floating point comparison linter
#'
#' Avoid equality comparisons to non-integer numeric literals, which are prone to
#' floating point representation issues. Prefer a tolerance check such as
#' `abs(x - 3.0) < eps`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x == 3.0",
#'   linters = floating_point_comparison_linter()
#' )
#'
#' lint(
#'   text = "y == 1/10",
#'   linters = floating_point_comparison_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x == 3L",
#'   linters = floating_point_comparison_linter()
#' )
#'
#' lint(
#'   text = "x == y",
#'   linters = floating_point_comparison_linter()
#' )
#'
#' @evalRd rd_tags("floating_point_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
floating_point_comparison_linter <- function() { # nolint: object_length_linter.
  lint_message <- paste(
    "Avoid direct comparisons to non-integer numeric literals.",
    "Use abs(x - 3.0) < eps instead of ==."
  )

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    if (is.null(xml)) return(list())

    comparison_expr <- xml_find_all(xml, "//expr[EQ]")
    if (length(comparison_expr) == 0L) return(list())

    lints <- list()
    for (expr in comparison_expr) {
      lhs <- xml_find_first(expr, "expr[1]")
      rhs <- xml_find_first(expr, "expr[2]")
      if (is_non_integer_numeric_literal_expr(lhs) || is_non_integer_numeric_literal_expr(rhs)) {
        lints <- c(lints, xml_nodes_to_lints(expr, source_expression, lint_message, type = "warning"))
      }
    }

    lints
  })
}

is_non_integer_numeric_literal_expr <- function(expr) { # nolint: object_length_linter.
  if (is.null(expr) || length(expr) == 0L) return(FALSE)
  if (length(xml_find_all(expr, ".//SYMBOL")) > 0L) return(FALSE)
  if (length(xml_find_all(expr, ".//SYMBOL_FUNCTION_CALL")) > 0L) return(FALSE)
  if (length(xml_find_all(expr, ".//STR_CONST")) > 0L) return(FALSE)

  numeric_nodes <- xml_find_all(expr, ".//NUM_CONST")
  if (length(numeric_nodes) == 0L) return(FALSE)

  if (length(xml_find_all(expr, ".//OP-SLASH")) > 0L) return(TRUE)

  numeric_text <- xml_text(numeric_nodes)
  any(vapply(numeric_text, is_non_integer_numeric_literal, logical(1L)))
}

is_non_integer_numeric_literal <- function(text) {
  if (text %in% c("NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")) {
    return(FALSE)
  }
  if (endsWith(text, "i")) return(FALSE)
  if (endsWith(text, "L")) return(FALSE)
  grepl("[.eE]", text)
}
