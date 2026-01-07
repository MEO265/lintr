#' Non-integer numeric comparison linter
#'
#' Comparing floating-point values for equality is fragile and can be affected
#'   by rounding error. Prefer checking whether the absolute difference is within
#'   an epsilon tolerance.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x == 3.0",
#'   linters = non_integer_comparison_linter()
#' )
#'
#' lint(
#'   text = "y == 1/10",
#'   linters = non_integer_comparison_linter()
#' )
#'
#' lint(
#'   text = "3.0 == x",
#'   linters = non_integer_comparison_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x == 3L",
#'   linters = non_integer_comparison_linter()
#' )
#'
#' @evalRd rd_tags("non_integer_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
non_integer_comparison_linter <- function() {
  non_integer_const <- "
    NUM_CONST[
      not(starts-with(text(), 'NA'))
      and not(substring(text(), string-length(text())) = 'L')
      and (
        contains(text(), '.')
        or contains(translate(text(), 'E', 'e'), 'e')
      )
    ]
  "

  numeric_literal <- "
    NUM_CONST
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[NUM_CONST]
    )
  "

  non_integer_division <- glue::glue("
    (OP-SLASH or OP-DIV)
    and count(expr) = 2
    and expr[1][{numeric_literal}]
    and expr[2][{numeric_literal}]
  ")

  non_integer_expr <- glue::glue("
    {non_integer_const}
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[{non_integer_const}]
    )
    or (
      {non_integer_division}
    )
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[{non_integer_division}]
    )
  ")

  xpath <- glue::glue("
  //EQ
    /parent::expr[
      expr[{non_integer_expr}]
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = "Avoid equality comparisons with non-integer numerics; use abs(x - y) < eps instead.",
      type = "warning"
    )
  })
}
