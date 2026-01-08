test_that("leap_year_linter detects as.integer(x * 365.25)", {
  linter <- leap_year_linter()
  lint_msg <- rex::rex(
    "Use",
    anything,
    "1461L",
    anything,
    "%/% 4L",
    anything,
    "as.integer",
    anything,
    "365.25"
  )

  expect_lint("as.integer(x * 365.25)", lint_msg, linter)
  expect_lint("as.integer(365.25 * x)", lint_msg, linter)
  expect_lint("as.integer((x + y) * 365.25)", lint_msg, linter)
})

test_that("leap_year_linter suggests reduced fractions", {
  linter <- leap_year_linter()

  expect_lint(
    "as.integer(x * 0.1)",
    rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "10L"),
    linter
  )
  expect_lint(
    "as.integer(0.125 * x)",
    rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "8L"),
    linter
  )
  expect_lint("as.integer(x * 1e-3)", NULL, linter)
})

test_that("leap_year_linter skips other conversions", {
  linter <- leap_year_linter()

  expect_lint("as.integer(x * 365)", NULL, linter)
  expect_lint("as.integer(x * 365.25 + 1)", NULL, linter)
  expect_lint("as.integer(x * 365.0)", NULL, linter)
  expect_lint("as.integer(x * 1e3)", NULL, linter)
  expect_lint("as.numeric(x * 365.25)", NULL, linter)
})

test_that("leap_year_linter suggests fractions beyond leap-year example", {
  linter <- leap_year_linter()

  expect_lint(
    "as.integer(x * 365.2501)",
    rex::rex("as.integer", anything, "3652501L", anything, "%/%", anything, "10000L"),
    linter
  )
})
