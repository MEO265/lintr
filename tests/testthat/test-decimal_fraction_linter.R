test_that("decimal_fraction_linter detects as.integer(x * 365.25)", {
  linter <- decimal_fraction_linter()
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

test_that("decimal_fraction_linter suggests reduced fractions", {
  linter <- decimal_fraction_linter()

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
  expect_lint(
    "as.integer(x * 3.2)",
    rex::rex("as.integer", anything, "16L", anything, "%/%", anything, "5L"),
    linter
  )
  expect_lint(
    "as.integer(x * 0.14)",
    rex::rex("as.integer", anything, "7L", anything, "%/%", anything, "50L"),
    linter
  )
  expect_lint(
    "as.integer(x * 7.001)",
    rex::rex("as.integer", anything, "7001L", anything, "%/%", anything, "1000L"),
    linter
  )
  expect_lint(
    "as.integer(x * 3e-5)",
    rex::rex("as.integer", anything, "3L", anything, "%/%", anything, "100000L"),
    linter
  )
  expect_lint(
    "as.integer(x * 1e-3)",
    rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "1000L"),
    linter
  )
})

test_that("decimal_fraction_linter skips other conversions", {
  linter <- decimal_fraction_linter()

  expect_lint("as.integer(x * 365)", NULL, linter)
  expect_lint("as.integer(x * 3.0)", NULL, linter)
  expect_lint("as.integer(x * 3)", NULL, linter)
  expect_lint("as.integer(x * 3L)", NULL, linter)
  expect_lint("as.integer(x * 365.25 + 1)", NULL, linter)
  expect_lint("as.integer(x * 365.0)", NULL, linter)
  expect_lint("as.integer(x * 1e3)", NULL, linter)
  expect_lint("as.numeric(x * 365.25)", NULL, linter)
})

test_that("decimal_fraction_linter suggests fractions beyond leap-year example", {
  linter <- decimal_fraction_linter()

  expect_lint(
    "as.integer(x * 365.2501)",
    rex::rex("as.integer", anything, "3652501L", anything, "%/%", anything, "10000L"),
    linter
  )
})
