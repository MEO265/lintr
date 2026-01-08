test_that("decimal_fraction_linter flags decimal fractions", {
  linter <- decimal_fraction_linter()

  cases <- list(
    "as.integer(x * 365.25)" = rex::rex(
      "Use",
      anything,
      "1461L",
      anything,
      "%/% 4L",
      anything,
      "as.integer",
      anything,
      "365.25"
    ),
    "as.integer(365.25 * x)" = rex::rex("1461L", anything, "%/% 4L"),
    "as.integer((x + y) * 365.25)" = rex::rex("1461L", anything, "%/% 4L"),
    "as.integer(x * 0.1)" = rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "10L"),
    "as.integer(0.125 * x)" = rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "8L"),
    "as.integer(x * 3.2)" = rex::rex("as.integer", anything, "16L", anything, "%/%", anything, "5L"),
    "as.integer(x * 0.14)" = rex::rex("as.integer", anything, "7L", anything, "%/%", anything, "50L"),
    "as.integer(x * 7.001)" = rex::rex("as.integer", anything, "7001L", anything, "%/%", anything, "1000L"),
    "as.integer(x * 365.2501)" = rex::rex(
      "as.integer",
      anything,
      "3652501L",
      anything,
      "%/%",
      anything,
      "10000L"
    )
  )

  for (input in names(cases)) {
    expect_lint(input, cases[[input]], linter)
  }
})

test_that("decimal_fraction_linter flags negative exponent fractions", {
  linter <- decimal_fraction_linter()

  cases <- list(
    "as.integer(x * 3e-5)" = rex::rex("as.integer", anything, "3L", anything, "%/%", anything, "100000L"),
    "as.integer(x * 1e-3)" = rex::rex("as.integer", anything, "1L", anything, "%/%", anything, "1000L")
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
    "as.numeric(x * 365.25)"
  )

  for (input in cases) {
    expect_lint(input, NULL, linter)
  }
})
