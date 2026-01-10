test_that("decimal_fraction_linter flags decimal fractions", {
  linter <- decimal_fraction_linter()

  cases <- list(
    "as.integer(x * 365.25)" = rex::rex("Use", anything, "1461L", anything, "/ 4L"),
    "as.integer(365.25 * x)" = rex::rex("1461L", anything, "/ 4L"),
    "as.integer((x + y) * 365.25)" = rex::rex("1461L", anything, "/ 4L"),
    "as.integer(x * 0.1)" = rex::rex("as.integer", anything, "/", anything, "10L"),
    "as.integer(0.125 * x)" = rex::rex("as.integer", anything, "/", anything, "8L"),
    "as.integer(x * 3.2)" = rex::rex("as.integer", anything, "16L", anything, "/", anything, "5L"),
    "as.integer(x * 0.14)" = rex::rex("as.integer", anything, "7L", anything, "/", anything, "50L"),
    "as.integer(x * -0.14)" = rex::rex("as.integer", anything, "-7L", anything, "/", anything, "50L"),
    "as.integer(x * 7.001)" = rex::rex("as.integer", anything, "7001L", anything, "/", anything, "1000L"),
    "as.integer(x / 0.13)" = rex::rex("as.integer", anything, "100L", anything, "/", anything, "13L"),
    "as.integer(x / 0.5)" = rex::rex("as.integer", anything, "2L"),
    "as.integer(x * 365.2501)" = rex::rex("as.integer", anything, "3652501L", anything, "/", anything, "10000L"),
    "ceiling(x * 365.25)" = rex::rex("ceiling", anything, "1461L", anything, "/ 4L"),
    "floor(x / 0.13)" = rex::rex("floor", anything, "100L", anything, "/ 13L"),
    "trunc(x * 0.14)" = rex::rex("trunc", anything, "7L", anything, "/", anything, "50L")
  )

  for (input in names(cases)) {
    expect_lint(input, cases[[input]], linter)
  }
})

test_that("decimal_fraction_linter flags negative exponent fractions", {
  linter <- decimal_fraction_linter()

  cases <- list(
    "as.integer(x * 3e-5)" = rex::rex("as.integer", anything, "3L", anything, "/", anything, "100000L"),
    "as.integer(x * 1e-3)" = rex::rex("as.integer", anything, "/", anything, "1000L")
  )

  for (input in names(cases)) {
    expect_lint(input, cases[[input]], linter)
  }
})

test_that("decimal_fraction_linter skips integer-like conversions", {
  linter <- decimal_fraction_linter()

  cases <- c(
    "as.integer(x * 365)",
    "as.integer(x * 3.0)",
    "as.integer(x * 3)",
    "as.integer(x * 3L)",
    "as.integer(x * 365.0)",
    "as.integer(x * 1e3)",
    "as.integer(x * 3.2e2)",
    "as.integer(0.13 / x)",
    "as.numeric(x * 365.25)"
  )

  for (input in cases) {
    expect_lint(input, NULL, linter)
  }
})

test_that("decimal_fraction_linter helpers parse fractions", {
  expect_identical(float_to_fraction("3.2"), list(numerator = 16L, denominator = 5L))
  expect_identical(float_to_fraction("0.14"), list(numerator = 7L, denominator = 50L))
  expect_identical(float_to_fraction("-0.14"), list(numerator = -7L, denominator = 50L))
  expect_identical(float_to_fraction("7.001"), list(numerator = 7001L, denominator = 1000L))
  expect_identical(float_to_fraction("3e-5"), list(numerator = 3L, denominator = 100000L))
  expect_identical(float_to_fraction("1e-3"), list(numerator = 1L, denominator = 1000L))
})

test_that("decimal_fraction_linter helpers skip integer-like inputs", {
  expect_null(float_to_fraction("3"))
  expect_null(float_to_fraction("3L"))
  expect_null(float_to_fraction("3.0"))
  expect_null(float_to_fraction("1e3"))
})

test_that("decimal_fraction_linter helpers reduce and reject fractions", {
  expect_identical(normalize_fraction(12L, 20L), list(numerator = 3L, denominator = 5L))
  expect_null(normalize_fraction(1L, 1L))
  expect_null(normalize_fraction(1L, 0L))
  expect_identical(int_gcd(18L, 12L), 6L)
})

test_that("decimal_fraction_linter lint points at number start", {
  linter <- decimal_fraction_linter()

  expect_lint(
    "as.integer(x * 0.14)",
    list(
      message = rex::rex("7L", anything, "50L"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
  expect_lint(
    "as.integer(x / 0.13)",
    list(
      message = rex::rex("100L", anything, "13L"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
})

test_that("decimal_fraction_linter drops redundant integer operations", {
  linter <- decimal_fraction_linter()

  lints <- lint(text = "as.integer(x * 0.1)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("* 1L", message, fixed = TRUE))
  expect_false(grepl("/ 1L", message, fixed = TRUE))

  lints <- lint(text = "as.integer(x / 0.5)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("/ 1L", message, fixed = TRUE))
})
