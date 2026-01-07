test_that("floating point comparisons are linted", {
  linter <- floating_point_comparison_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lints <- function(text) {
    lints <- lint(text = text, linters = linter)
    testthat::expect_length(lints, 1L)
    testthat::expect_match(lints[[1L]]$message, lint_msg)
  }

  expect_lints("x == 3.0")
  expect_lints("3.0 == x")
  expect_lints("y == 1/10")
  expect_lints("y == -1/10")
  expect_lints("y == 1e-3")
  expect_lints("x == 3")
})

test_that("integer comparisons are not linted", {
  linter <- floating_point_comparison_linter()

  expect_no_lints <- function(text) {
    lints <- lint(text = text, linters = linter)
    testthat::expect_length(lints, 0L)
  }

  expect_no_lints("x == 3L")
  expect_no_lints("x == -3L")
  expect_no_lints("x == 1e3L")
  expect_no_lints("x == 10%/%2")
})
