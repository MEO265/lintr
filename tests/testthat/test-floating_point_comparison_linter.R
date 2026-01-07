test_that("floating point comparisons are linted", {
  linter <- floating_point_comparison_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lint("x == 3.0", lint_msg, linters = linter)
  expect_lint("3.0 == x", lint_msg, linters = linter)
  expect_lint("y == 1/10", lint_msg, linters = linter)
  expect_lint("y == -1/10", lint_msg, linters = linter)
  expect_lint("y == 1e-3", lint_msg, linters = linter)
  expect_lint("x == 3", lint_msg, linters = linter)
})

test_that("integer comparisons are not linted", {
  linter <- floating_point_comparison_linter()

  expect_no_lint("x == 3L", linters = linter)
  expect_no_lint("x == -3L", linters = linter)
  expect_no_lint("x == 1e3L", linters = linter)
  expect_no_lint("x == 10%/%2", linters = linter)
})

test_that("lint metadata points to the comparison expression", {
  linter <- floating_point_comparison_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"
  expect_lint(
    c("x <- 1", "x == 3"),
    list(message = lint_msg, line_number = 2L, column_number = 1L),
    linters = linter
  )
})
