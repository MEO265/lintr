test_that("numerical_linter flags decimal fraction multipliers/divisors", {
  linter <- numerical_linter()

  expect_lint(
    "as.integer(x * 0.1)",
    rex::rex("Use", anything, "as.integer", anything, "/", anything, "10L", anything, "avoid floating-point rounding"),
    linter
  )
  expect_lint(
    "as.integer(x * 3.2)",
    rex::rex(
      "Use", anything, "as.integer", anything, "16L", anything, "/ 5L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 0.14)",
    rex::rex(
      "Use", anything, "as.integer", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * -0.14)",
    rex::rex(
      "Use", anything, "as.integer", anything, "-7L", anything, "/ 50L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 7.001)",
    rex::rex("as.integer", anything, "7001L", anything, "/", anything, "1000L"),
    linter
  )
  expect_lint(
    "as.integer(x / 0.13)",
    rex::rex(
      "Use", anything, "as.integer", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 365.2501)",
    rex::rex("as.integer", anything, "3652501L", anything, "/", anything, "10000L"),
    linter
  )
  expect_lint(
    "ceiling(x * 3.2)",
    rex::rex(
      "Use", anything, "ceiling", anything, "16L", anything, "/ 5L",
      anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "floor(x / 0.13)",
    rex::rex("Use", anything, "floor", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"),
    linter
  )
  expect_lint(
    "trunc(x * 0.14)",
    rex::rex("Use", anything, "trunc", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"),
    linter
  )
})

test_that("numerical_linter flags negative exponent fractional literals", {
  linter <- numerical_linter()

  expect_lint(
    "as.integer(x * 3e-5)",
    rex::rex(
      "Use", anything, "as.integer", anything, "3L", anything, "/ 100000L", anything, "avoid floating-point rounding"
    ),
    linter
  )
  expect_lint(
    "as.integer(x * 1e-3)",
    rex::rex("Use", anything, "as.integer", anything, "/ 1000L", anything, "avoid floating-point rounding"),
    linter
  )
})

test_that("numerical_linter skips integer-like multipliers/divisors", {
  linter <- numerical_linter()
  linter_exact <- numerical_linter(lint_exact_binary = TRUE)

  expect_no_lint("as.integer(x * 365)", linter)
  expect_no_lint("as.integer(x * 3.0)", linter)
  expect_no_lint("as.integer(0.125 * x)", linter)
  expect_no_lint("as.integer(x * 1.25)", linter)
  expect_no_lint("as.integer(x * 3)", linter)
  expect_no_lint("as.integer(x * 3L)", linter)
  expect_no_lint("as.integer(x * 365.0)", linter)
  expect_no_lint("as.integer(x * 365.25)", linter)
  expect_no_lint("as.integer(365.25 * x)", linter)
  expect_no_lint("as.integer((x + y) * 365.25)", linter)
  expect_no_lint("ceiling(x * 365.25)", linter)
  expect_no_lint("as.integer(x * 12.25)", linter)
  expect_no_lint("as.integer(x * 1e3)", linter)
  expect_no_lint("as.integer(x * 3.2e2)", linter)
  expect_no_lint("as.integer(x * NaN)", linter)
  expect_no_lint("as.integer(x * Inf)", linter)
  expect_no_lint("as.integer(x / 0.5)", linter)
  expect_no_lint("as.integer(x / Inf)", linter)
  expect_no_lint("as.integer(0.13 / x)", linter)
  expect_no_lint("as.numeric(x * 365.25)", linter)
  expect_lint(
    "as.integer(0.125 * x)",
    list(rex::rex("Use", anything, "8L", anything, "avoid floating-point rounding")),
    linter_exact
  )
  expect_lint(
    "as.integer(x * 0.5)",
    list(rex::rex("Use", anything, "as.integer", anything, "x / 2L")),
    linter_exact
  )
  expect_lint(
    "as.integer(x / 0.25)",
    list(rex::rex("Use", anything, "as.integer", anything, "4L")),
    linter_exact
  )
})

test_that("numerical_linter respects lint_exact_binary", {
  linter <- numerical_linter()
  linter_exact <- numerical_linter(lint_exact_binary = TRUE)

  expect_no_lint("as.integer(x * 0.5)", linter)
  expect_lint(
    "as.integer(x * 0.5)",
    list(rex::rex("Use", anything, "as.integer", anything, "x / 2L")),
    linter_exact
  )
  expect_no_lint("as.integer(x / 0.25)", linter)
  expect_lint(
    "as.integer(x / 0.25)",
    list(rex::rex("Use", anything, "as.integer", anything, "4L")),
    linter_exact
  )
  expect_no_lint("as.integer(1.25 * x)", linter)
  expect_lint(
    "as.integer(1.25 * x)",
    list(rex::rex("Use", anything, "5L", anything, "/ 4L")),
    linter_exact
  )
})

test_that("numerical_linter lint ranges start at decimal literal", {
  linter <- numerical_linter()

  expect_lint(
    "as.integer(x * 0.14)",
    list(
      rex::rex("Use", anything, "7L", anything, "/ 50L", anything, "avoid floating-point rounding"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
  expect_lint(
    "as.integer(x / 0.13)",
    list(
      rex::rex("Use", anything, "100L", anything, "/ 13L", anything, "avoid floating-point rounding"),
      line_number = 1L,
      column_number = 16L,
      ranges = list(c(16L, 19L))
    ),
    linter
  )
})

test_that("numerical_linter drops redundant integer factors in messages", {
  linter <- numerical_linter()

  lints <- lint(text = "as.integer(x * 0.1)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("* 1L", message, fixed = TRUE))
  expect_false(grepl("/ 1L", message, fixed = TRUE))

  lints <- lint(text = "as.integer(x / 0.2)", linters = linter)
  message <- lints[[1L]]$message
  expect_false(grepl("/ 1L", message, fixed = TRUE))
})

test_that("floating point comparisons are linted", {
  linter <- numerical_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lint("x == 3.0", lint_msg, linters = linter)
  expect_lint("3.0 == x", lint_msg, linters = linter)
  expect_lint("y == 1/10", lint_msg, linters = linter)
  expect_lint("y == -1/10", lint_msg, linters = linter)
  expect_lint("y == 1e-3", lint_msg, linters = linter)
  expect_lint("x == 3", lint_msg, linters = linter)
  expect_lint("x == c(1, y, Z)", lint_msg, linters = linter)
  expect_lint("c(1, y, Z) == x", lint_msg, linters = linter)
  expect_lint("x %in% c(1, y, Z)", lint_msg, linters = linter)
  expect_lint("c(1, y, Z) %in% x", lint_msg, linters = linter)
  expect_lint("match(x, c(1, y, Z))", lint_msg, linters = linter)
  expect_lint("match(c(1, y, Z), x)", lint_msg, linters = linter)
})

test_that("integer comparisons are not linted", {
  linter <- numerical_linter()

  expect_no_lint("x == 3L", linters = linter)
  expect_no_lint("x == -3L", linters = linter)
  expect_no_lint("x == 1e3L", linters = linter)
  expect_no_lint("x == 10%/%2", linters = linter)
  expect_no_lint("x == c(1L, y, Z)", linters = linter)
  expect_no_lint("c(1L, y, Z) == x", linters = linter)
  expect_no_lint("x %in% c(1L, y, Z)", linters = linter)
  expect_no_lint("c(1L, y, Z) %in% x", linters = linter)
  expect_no_lint("match(x, c(1L, y, Z))", linters = linter)
  expect_no_lint("match(c(1L, y, Z), x)", linters = linter)
  expect_no_lint("x == Inf", linters = linter)
  expect_no_lint("x == -Inf", linters = linter)
  expect_no_lint("x == TRUE", linters = linter)
  expect_no_lint("x == FALSE", linters = linter)
  expect_no_lint("match(TRUE, x)", linters = linter)
})

test_that("integer literals can be ignored when configured", {
  linter <- numerical_linter(lint_implicit_integer = FALSE)
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_no_lint("x == 3", linters = linter)
  expect_no_lint("x == c(1, y, Z)", linters = linter)
  expect_lint("x == 3.0", lint_msg, linters = linter)
  expect_lint("x == 1e-3", lint_msg, linters = linter)
})

test_that("lint metadata points to the comparison expression", {
  linter <- numerical_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lint(
    "x == 3",
    list(lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "3 == x",
    list(lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "x == 1/10",
    list(lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "1/10 == x",
    list(lint_msg, line_number = 1L, column_number = 6L),
    linters = linter
  )
  expect_lint(
    "x %in% c(1, y, Z)",
    list(lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "match(x, c(1, y, Z))",
    list(lint_msg, line_number = 1L, column_number = 1L),
    linters = linter
  )
})

test_that("stable arithmetic replacements handle equivalent syntax", {
  linter <- numerical_linter()
  cases <- list(
    c("log(1 + x)", "log1p(x)"),
    c("log(x + 1L)", "log1p(x)"),
    c("base::log(x = (1e0 + (x)))", "log1p((x))"),
    c("base:::log(0x1 + x)", "log1p(x)"),
    c("log(+1 + x)", "log1p(x)"),
    c("log(1 - (x + y))", "log1p(-((x + y)))"),
    c("exp(x) - 1", "expm1(x)"),
    c("(base::exp(x = x + y)) - (1.0)", "expm1(x + y)"),
    c("1L - exp(x)", "-expm1(x)"),
    c("1 / (1 + exp(-x))", "stats::plogis(x)"),
    c("1 / (base::exp(-(x + y)) + 1)", "stats::plogis((x + y))"),
    c("1 / (1 + exp(x + y))", "stats::plogis(-(x + y))"),
    c("exp(x) / (1 + exp(x))", "stats::plogis(x)"),
    c("base::exp(x = (x)) / (base::exp(x) + 1L)", "stats::plogis(x)"),
    c("log(p / (1 - p))", "stats::qlogis(p)"),
    c("log((p) / (1 - (p)))", "stats::qlogis(p)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
})

test_that("special logarithmic functions respect signs and argument matching", {
  linter <- numerical_linter()
  cases <- list(
    c("log(beta(a, b))", "lbeta(a = a, b = b)"),
    c("base::log(base::beta(b = y, a = x))", "lbeta(a = x, b = y)"),
    c("log(beta(a, a = b))", "lbeta(a = b, b = a)"),
    c("log(abs(gamma(x)))", "lgamma(x = x)"),
    c("log(base::abs(base::factorial(x)))", "lfactorial(x = x)"),
    c("log(abs(choose(n, k)))", "lchoose(n = n, k = k)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
  for (code in c("log(gamma(x))", "log(factorial(x))", "log(choose(n, k))")) {
    expect_no_lint(code, linter)
    expect_lint(code, "only where.*non-negative.*absolute value", numerical_linter(include_heuristics = TRUE))
  }
})

test_that("density and probability replacements preserve matched arguments", {
  linter <- numerical_linter()
  cases <- list(
    c("log(dnorm(x))", "stats::dnorm(x = x, log = TRUE)"),
    c("log(stats::dnorm(x, 2, 3, FALSE))", "stats::dnorm(x = x, mean = 2, sd = 3, log = TRUE)"),
    c("log(dnorm(sd = 3, x = x, log = FALSE, mean = 2))", "stats::dnorm(x = x, mean = 2, sd = 3, log = TRUE)"),
    c("log(dgamma(x, shape = a, scale = s))", "stats::dgamma(x = x, shape = a, scale = s, log = TRUE)"),
    c("log(dnbinom(x, size = s, mu = m))", "stats::dnbinom(x = x, size = s, mu = m, log = TRUE)"),
    c("log(pnorm(x))", "stats::pnorm(q = x, log.p = TRUE)"),
    c("log(pnorm(x, 0, 1, FALSE, FALSE))", "stats::pnorm(q = x, mean = 0, sd = 1, lower.tail = FALSE, log.p = TRUE)"),
    c("1 - pnorm(x)", "stats::pnorm(q = x, lower.tail = FALSE)"),
    c("1 - stats::pnorm(q = x, lower = FALSE)", "stats::pnorm(q = x, lower.tail = TRUE)"),
    c("log(1 - pnorm(x))", "stats::pnorm(q = x, lower.tail = FALSE, log.p = TRUE)"),
    c("log((1 - pnorm(x, lower.tail = FALSE)))", "stats::pnorm(q = x, lower.tail = TRUE, log.p = TRUE)"),
    c("qnorm(1 - p)", "stats::qnorm(p = p, lower.tail = FALSE)"),
    c("stats::qnorm(sd = s, p = 1 - p, lower.tail = FALSE)", "stats::qnorm(p = p, sd = s, lower.tail = TRUE)"),
    c("qnorm(1 - p, 2, 3, FALSE, FALSE)", "stats::qnorm(p = p, mean = 2, sd = 3, lower.tail = TRUE, log.p = FALSE)"),
    c("qgamma(1 - p, shape = a, scale = s)", "stats::qgamma(p = p, shape = a, scale = s, lower.tail = FALSE)")
  )
  for (case in cases) expect_lint(case[1L], rex::rex(case[2L]), linter)
})

test_that("partial argument names work when partial matching warnings are errors", {
  withr::local_options(warn = 2L, warnPartialMatchArgs = TRUE)
  expect_lint(
    "1 - stats::pnorm(q = x, lower = FALSE)",
    rex::rex("stats::pnorm(q = x, lower.tail = TRUE)"),
    numerical_linter()
  )
})

test_that("standard distribution signatures are covered without prefix guessing", {
  linter <- numerical_linter()
  distributions <- c(
    beta = "x, 1, 2", binom = "x, 2, 0.5", cauchy = "x", chisq = "x, 2", exp = "x",
    f = "x, 2, 3", gamma = "x, 2", geom = "x, 0.5", hyper = "x, 2, 3, 1", lnorm = "x",
    logis = "x", nbinom = "x, 2, 0.5", norm = "x", pois = "x, 2", signrank = "x, 2",
    t = "x, 2", unif = "x", weibull = "x, 2", wilcox = "x, 2, 3"
  )
  for (distribution in names(distributions)) {
    arguments <- distributions[[distribution]]
    expect_lint(paste0("log(d", distribution, "(", arguments, "))"), "log = TRUE", linter)
    expect_lint(paste0("log(p", distribution, "(", arguments, "))"), "log.p = TRUE", linter)
    expect_lint(paste0("1 - p", distribution, "(", arguments, ")"), "lower.tail = FALSE", linter)
    expect_lint(paste0("q", distribution, "(1 - ", arguments, ")"), "lower.tail = FALSE", linter)
  }
  expect_lint("log(ptukey(x, 3, 10))", "log.p = TRUE", linter)
  expect_lint("qtukey(1 - p, 3, 10)", "lower.tail = FALSE", linter)
  expect_no_lint("log(dcustom(x))", linter)
  expect_no_lint("1 - pcustom(x)", linter)
  expect_no_lint("qcustom(1 - p)", linter)
})

test_that("already stable calls and unsafe near misses are ignored", {
  linter <- numerical_linter()
  cases <- c(
    "log1p(x)", "expm1(x)", "plogis(x)", "qlogis(p)", "lbeta(a, b)", "lgamma(x)",
    "log(x)", "log(2 + x)", "log(x - 1)", "log(1 + x + y)", "log(1 / (1 - p))",
    "log(1 + x, 10)", "log(1 + x, base = exp(1))", "log(x = 1 + x, b = 10)",
    "log10(1 + x)", "log2(1 + x)", "exp(x) - 2", "2 - exp(x)", "exp(x) + 1",
    "exp(x) / (1 + exp(y))", "2 / (1 + exp(-x))", "1 / (2 + exp(-x))",
    "exp(f()) / (1 + exp(f()))", "log(f() / (1 - f()))", "log(p / (1 - q))",
    "log(dnorm(x, log = TRUE))", "log(dnorm(x, 0, 1, TRUE))", "log(dnorm(x, log = flag))",
    "log(pnorm(x, log.p = TRUE))", "log(pnorm(x, log.p = flag))",
    "1 - pnorm(x, log.p = TRUE)", "1 - pnorm(x, log.p = flag)",
    "1 - pnorm(x, lower.tail = flag)", "1 - pnorm(x, lower.tail = T)",
    "qnorm(1 - p, log.p = TRUE)", "qnorm(1 - p, log.p = flag)", "qnorm(1 - p, lower.tail = flag)",
    "other::log(1 + x)", "other::exp(x) - 1", "log(other::dnorm(x))", "1 - other::pnorm(x)",
    "other::qnorm(1 - p)", "log(other::beta(a, b))", "1 / (1 + other::exp(-x))",
    "log(1 + 1i)", "exp(1i) - 1", "1 / (1 + exp(1i))", "log(1 + complex(real = x))",
    "log()", "log(x = )", "log(1 + x, extra = 1)", "log(1 + x, x = y)",
    "log(...)", "log(1 + x, ...)", "exp() - 1", "exp(...)-1", "exp(x, y) - 1",
    "log(dnorm())", "log(dnorm(x, ))", "log(dnorm(x, ...))", "log(dnorm(x, unknown = y))",
    "log(dbeta(x))", "log(beta(a))", "log(beta(a = x, a = y))", "qnorm()", "qnorm(p = )",
    "log(abs(gamma()))", "log(sum())", "log(sum(exp(x), na.rm = TRUE))",
    "# log(1 + x)", "'exp(x) - 1'", "obj$log(1 + x)", "obj$exp(x) - 1"
  )
  for (code in cases) expect_no_lint(code, linter)
})

test_that("configuration separates real numeric assumptions and heuristics", {
  linter <- numerical_linter(assume_real_numeric = FALSE)
  for (code in c("log(1 + x)", "exp(x) - 1", "1 / (1 + exp(-x))", "log(p / (1 - p))")) {
    expect_no_lint(code, linter)
  }
  expect_lint("log(pnorm(x))", "log.p = TRUE", linter)
  expect_lint("log(beta(a, b))", "lbeta", linter)
  for (code in c("log(sum(exp(x)))", "log(exp(a) + exp(b))")) {
    expect_no_lint(code, numerical_linter())
    expect_lint(
      code, "Consider a stable log-sum-exp.*check empty inputs",
      numerical_linter(include_heuristics = TRUE)
    )
  }
  for (invalid in list(NULL, NA, c(TRUE, FALSE), 1L, "yes")) {
    expect_error(numerical_linter(assume_real_numeric = invalid))
    expect_error(numerical_linter(include_heuristics = invalid))
  }
})

test_that("comments, parentheses, nesting, and positions are handled", {
  linter <- numerical_linter()
  expect_lint(trim_some("
    log(1 + # comment
      x)
  "), rex::rex("log1p(x)"), linter)
  expect_lint(trim_some("
    exp(# comment
      x) - 1
  "), rex::rex("expm1(x)"), linter)
  expect_lint(trim_some("
    log(1 - pnorm(# comment
      x))
  "), "lower.tail = FALSE, log.p = TRUE", linter)
  expect_lint(
    trim_some("
    {
      a <- log(1 + x)
      b <- exp(y) - 1
      c <- log(1 - pnorm(z))
    }
    "),
    list(
      list("log1p", line_number = 2L, column_number = 8L, ranges = list(c(8L, 17L))),
      list("expm1", line_number = 3L, column_number = 8L),
      list("log.p = TRUE", line_number = 4L, column_number = 8L)
    ),
    linter
  )
  expect_lint("f <- function(x) log(1 + x)", "log1p", linter)
  expect_lint("log(1 + log(1 + y))", list("log1p", "log1p"), linter)
  expect_lint("exp(log(1 + y)) - 1", list("expm1", "log1p"), linter)
  expect_no_lint("x |> log()", linter)
  expect_lint("x |> (function(y) log(1 + y))()", "log1p", linter)
})

test_that("linting never evaluates input expressions", {
  linter <- numerical_linter()
  expect_lint("log(1 + stop('must not run'))", "log1p", linter)
  expect_lint("exp(stop('must not run')) - 1", "expm1", linter)
  expect_lint("log(1 + f(, y))", "log1p", linter)
  expect_lint("log(dnorm(stop('must not run')))", "log = TRUE", linter)
  expect_no_lint("log(dnorm(x, log = stop('must not run')))", linter)
})

test_that("suggested expressions remain useful at numerical extremes", {
  linter <- numerical_linter()
  cases <- list(
    list(code = "log(1 + x)", values = list(x = 1e-20), expected = log1p(1e-20)),
    list(code = "exp(x) - 1", values = list(x = 1e-20), expected = expm1(1e-20)),
    list(code = "exp(x) / (1 + exp(x))", values = list(x = 1000.0), expected = 1.0),
    list(code = "1 - pnorm(x)", values = list(x = 10.0), expected = pnorm(10.0, lower.tail = FALSE)),
    list(code = "log(pnorm(x))", values = list(x = -40.0), expected = pnorm(-40.0, log.p = TRUE)),
    list(code = "log(dnorm(x))", values = list(x = 50.0), expected = dnorm(50.0, log = TRUE)),
    list(code = "log(beta(a, b))", values = list(a = 1000.0, b = 1000.0), expected = lbeta(1000.0, 1000.0)),
    list(code = "qnorm(1 - p)", values = list(p = 1e-20), expected = qnorm(1e-20, lower.tail = FALSE))
  )
  for (case in cases) {
    lint_message <- lint(text = case$code, linters = linter)[[1L]]$message
    replacement <- sub("^Use (.*) for better numerical stability\\.$", "\\1", lint_message)
    observed <- eval(str2lang(replacement), envir = list2env(case$values))
    expect_identical(observed, case$expected)
    expect_true(is.finite(observed))
  }
})

test_that("decimal fractions are normalized through the public linter", {
  linter <- numerical_linter()
  expect_lint("as.integer(x * 1.23e1)", rex::rex("as.integer((x * 123L) / 10L)"), linter)
  expect_lint("as.integer(x * 1.2300e0)", rex::rex("as.integer((x * 123L) / 100L)"), linter)
  expect_lint("as.integer(x * 0.6)", rex::rex("as.integer((x * 3L) / 5L)"), linter)
})

test_that("one expression checks all numerical rule groups", {
  linter <- numerical_linter()
  expect_s3_class(linter, "linter")
  code <- "list(as.integer(x * 0.14), x == 0.1, log(1 - pnorm(x)))"
  expect_lint(
    code,
    list("avoid floating-point rounding", "Avoid equality comparisons", "lower.tail = FALSE, log.p = TRUE"),
    linter
  )
  lints <- lint(text = code, linters = linter)
  expect_identical(vapply(lints, `[[`, character(1L), "linter"), rep("numerical_linter", 3L))
})

test_that("configuration preserves the other numerical rule groups", {
  linter <- numerical_linter(
    lint_exact_binary = TRUE,
    lint_implicit_integer = FALSE,
    assume_real_numeric = FALSE,
    include_heuristics = TRUE
  )
  expect_lint("as.integer(x * 0.5)", rex::rex("as.integer(x / 2L)"), linter)
  expect_no_lint("x == 3", linter)
  expect_lint("x == 1/10", "Avoid equality comparisons", linter)
  expect_no_lint("log(1 + x)", linter)
  expect_lint("log(sum(exp(x)))", "Consider a stable log-sum-exp", linter)
  expect_lint("log(pnorm(x))", "log.p = TRUE", linter)
})

test_that("only the combined numerical linter is exported and registered", {
  exports <- getNamespaceExports("lintr")
  expect_true("numerical_linter" %in% exports)
  old_linters <- c("decimal_fraction_linter", "float_comparison_linter", "numerical_stability_linter")
  expect_false(any(old_linters %in% exports))
  registered <- available_linters(tags = NULL)$linter
  expect_true("numerical_linter" %in% registered)
  expect_false(any(old_linters %in% registered))
})
