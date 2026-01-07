test_that("floating_point_comparison_linter blocks non-integer numeric comparisons", {
  linter <- floating_point_comparison_linter()
  lint_msg <- rex::rex("Avoid direct comparisons to non%-integer numeric literals")

  expect_lint("x == 3.0", lint_msg, linter)
  expect_lint("3.0 == x", lint_msg, linter)
  expect_lint("y == 1/10", lint_msg, linter)
  expect_lint("1/10 == y", lint_msg, linter)
  expect_lint("x == -3.0", lint_msg, linter)
})

test_that("floating_point_comparison_linter skips allowed comparisons", {
  linter <- floating_point_comparison_linter()

  expect_lint("x == 3L", NULL, linter)
  expect_lint("x == 3", NULL, linter)
  expect_lint("x == y", NULL, linter)
  expect_lint("x == NA", NULL, linter)
  expect_lint("x == y / 10", NULL, linter)
})
