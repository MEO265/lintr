#' Check numerical comparisons, rounding, and stability
#'
#' Detect fragile floating-point equality comparisons, decimal factors before
#' integer conversion or rounding, and numerically unstable expressions.
#' The linted code is never evaluated; review suggestions before applying them.
#'
#' @param lint_exact_binary Logical, default `FALSE`. Also lint decimal
#'   factors that are exactly representable in binary before `as.integer()`,
#'   `ceiling()`, `floor()`, or `trunc()`.
#' @param lint_implicit_integer Logical, default `TRUE`. When `FALSE`, ignore
#'   plain integer literals without an `L` suffix in equality comparisons;
#'   decimal literals and fractional divisions are still checked.
#' @param assume_real_numeric Logical, default `TRUE`. Assume unknown inputs
#'   to arithmetic, `log()`, and `exp()` are ordinary real numeric vectors.
#'   Set to `FALSE` to disable the `log1p()`, `expm1()`, `plogis()`, and
#'   `qlogis()` rules. Explicit complex literals are always excluded from these
#'   rules. Before R 4.6.0, `log1p()` and `expm1()` do not support complex inputs.
#' @param include_heuristics Logical, default `FALSE`. Also report log-sum-exp
#'   expressions and direct logarithms of `gamma()`, `factorial()`, and
#'   `choose()`. Review the input domain: logarithmic counterparts of these
#'   special functions compute logarithms of absolute values.
#'
#' @section Checks:
#' * Equality comparisons (`==`, `%in%`, and `match()`) with non-integer
#'   numeric literals suggest a tolerance comparison.
#' * Multiplication or division by decimal literals before integer conversion
#'   or rounding suggests a reduced fraction, such as `as.integer(x / 10L)`
#'   instead of `as.integer(x * 0.1)`.
#' * `log(1 + x)`, `log(x + 1)`, and `log(1 - x)` suggest `log1p()`.
#' * `exp(x) - 1` and `1 - exp(x)` suggest `expm1()`.
#' * `1 / (1 + exp(-x))` and `exp(x) / (1 + exp(x))` suggest `stats::plogis()`.
#' * `log(p / (1 - p))` suggests `stats::qlogis()`.
#' * `log(beta(a, b))` suggests `lbeta()`. Logarithms of absolute values of
#'   `gamma()`, `factorial()`, and `choose()` suggest logarithmic APIs.
#' * `log(dnorm(x))` and analogous standard stats density calls suggest
#'   `log = TRUE`; distribution functions suggest `log.p = TRUE`.
#' * `1 - pnorm(x)` suggests the opposite `lower.tail` setting.
#'   `log(1 - pnorm(x))` combines this with `log.p = TRUE`.
#' * `qnorm(1 - p)` and analogous quantile calls suggest the opposite tail.
#'
#' @section Scope of stability suggestions:
#' Unqualified calls are assumed to refer to standard base/stats functions.
#' Explicit calls from other namespaces are ignored by the stability rules.
#' Local function masking and S3/S4 dispatch cannot be resolved statically.
#' Named, partially named, and positional arguments are matched against standard
#' signatures. Calls with missing arguments, `...`, or unknown log/tail flags
#' are skipped. Non-natural logarithms are skipped. Rewrites removing repeated
#' evaluations are restricted to repeated symbols or numeric constants.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x == 0.1",
#'   linters = numerical_linter()
#' )
#' lint(
#'   text = "as.integer(x * 0.1)",
#'   linters = numerical_linter()
#' )
#' lint(
#'   text = "log(1 + x)",
#'   linters = numerical_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "abs(x - 0.1) < eps",
#'   linters = numerical_linter()
#' )
#' lint(
#'   text = "as.integer(x / 10L)",
#'   linters = numerical_linter()
#' )
#' lint(
#'   text = "log1p(x)",
#'   linters = numerical_linter()
#' )
#'
#' @evalRd rd_tags("numerical_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
# nolint next: cyclocomp_linter. Helpers in factory inflate complexity (gaborcsardi/cyclocomp#30).
numerical_linter <- function(lint_exact_binary = FALSE, lint_implicit_integer = TRUE,
                             assume_real_numeric = TRUE, include_heuristics = FALSE) {
  integer_literal_filter <- if (lint_implicit_integer) {
    ""
  } else {
    "
      and (
        contains(text(), '.')
        or contains(text(), 'e')
        or contains(text(), 'E')
      )
    "
  }

  non_integer_const <- glue("
    NUM_CONST[
      not(starts-with(text(), 'NA'))
      and not(text() = 'TRUE')
      and not(text() = 'FALSE')
      and not(text() = 'Inf')
      and not(substring(text(), string-length(text())) = 'L')
      {integer_literal_filter}
    ]
  ")

  integer_division <- "
    SPECIAL[text() = '%/%']
  "

  numeric_literal <- "
    NUM_CONST
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[NUM_CONST]
    )
  "

  non_integer_division <- glue("
    (OP-SLASH or OP-DIV)
    and count(expr) = 2
    and expr[1][{numeric_literal}]
    and expr[2][{numeric_literal}]
  ")

  base_non_integer_expr <- glue("
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

  non_integer_c_call <- glue("
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
    and expr[{base_non_integer_expr}]
  ")

  non_integer_expr <- glue("
    {base_non_integer_expr}
    or (
      {non_integer_c_call}
    )
  ")

  comparison_xpath <- glue("
  (//EQ | //SPECIAL[text() = '%in%'])[
    parent::expr[
      expr[{non_integer_expr}]
      and not(expr[{integer_division}])
    ]
  ]
  | //expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'match']]
    and expr[{non_integer_expr}]
  ]
  /expr[1]/SYMBOL_FUNCTION_CALL
  ")

  stopifnot(
    is.logical(assume_real_numeric), length(assume_real_numeric) == 1L, !is.na(assume_real_numeric),
    is.logical(include_heuristics), length(include_heuristics) == 1L, !is.na(include_heuristics)
  )

  distributions <- c(
    "beta", "binom", "cauchy", "chisq", "exp", "f", "gamma", "geom", "hyper",
    "lnorm", "logis", "nbinom", "norm", "pois", "signrank", "t", "unif", "weibull", "wilcox"
  )
  density_names <- paste0("d", distributions)
  probability_names <- c(paste0("p", distributions), "ptukey")
  quantile_names <- c(paste0("q", distributions), "qtukey")
  call_names <- c("log", quantile_names)
  stability_xpath <- glue("
    //expr[
      OP-MINUS or OP-SLASH
      or expr/SYMBOL_FUNCTION_CALL[{xp_text_in_table(call_names)}]
    ]
  ")

  filter_decimal_fraction_inputs <- function(bad_expr, num_text, other_factor, operator, is_negative) {
    ok <- nzchar(other_factor)
    list(
      bad_expr = bad_expr[ok],
      num_text = num_text[ok],
      other_factor = other_factor[ok],
      operator = operator[ok],
      is_negative = is_negative[ok]
    )
  }

  filter_decimal_fractions <- function(bad_expr, other_factor, operator, is_negative, fractions) {
    has_fraction <- vapply(fractions, Negate(is.null), logical(1L))
    list(
      bad_expr = bad_expr[has_fraction],
      other_factor = other_factor[has_fraction],
      operator = operator[has_fraction],
      is_negative = is_negative[has_fraction],
      fractions = fractions[has_fraction]
    )
  }

  build_decimal_fraction_message <- function(fractions, call_names, other_factor, operator, is_negative) {
    vapply(seq_along(fractions), function(idx) {
      fraction <- fractions[[idx]]
      sign_factor <- ifelse(is_negative[[idx]], -1L, 1L)
      numerator <- sign_factor * fraction[["numerator"]]
      denominator <- fraction[["denominator"]]
      if (operator[[idx]] == "OP-SLASH") {
        numerator <- sign_factor * fraction[["denominator"]]
        denominator <- fraction[["numerator"]]
      }
      has_mult_part <- numerator != 1L
      has_div_part <- denominator != 1L
      mult_part <- if (has_mult_part) sprintf(" * %dL", numerator) else ""
      div_part <- if (has_div_part) sprintf(" / %dL", denominator) else ""
      replacement_template <- if (has_mult_part && has_div_part) "%s((%s%s)%s)" else "%s(%s%s%s)"
      replacement <- sprintf(
        replacement_template,
        call_names[[idx]],
        other_factor[[idx]],
        mult_part,
        div_part
      )
      sprintf(
        "Use %s to avoid floating-point rounding.",
        replacement
      )
    }, character(1L))
  }

  is_exact_binary_literal <- function(fractional_part) {
    if (fractional_part == "") {
      return(TRUE)
    }
    fractional_digits <- as.integer(fractional_part)

    digits <- nchar(fractional_part)
    v5 <- 0L
    while (fractional_digits %% 5L == 0L) {
      fractional_digits <- fractional_digits / 5L
      v5 <- v5 + 1L
    }

    v5 >= digits
  }

  extract_decimal_parts <- function(number) {
    split_exp <- strsplit(number, "[eE]", perl = TRUE)[[1L]]
    mantissa <- split_exp[[1L]]
    exponent <- if (length(split_exp) > 1L) as.integer(split_exp[[2L]]) else 0L
    sign_char <- if (grepl("^[-+]", mantissa)) substr(mantissa, 1L, 1L) else ""
    mantissa <- sub("^[-+]", "", mantissa)

    parts <- strsplit(mantissa, ".", fixed = TRUE)[[1L]]
    integer_part <- parts[[1L]]
    fractional_part <- if (length(parts) > 1L) parts[[2L]] else ""
    integer_part <- ifelse(nzchar(integer_part), integer_part, "0")

    digits_text <- paste0(integer_part, fractional_part)
    decimal_pos <- nchar(integer_part)
    new_decimal_pos <- decimal_pos + exponent
    if (new_decimal_pos >= nchar(digits_text)) {
      integer_part <- paste0(digits_text, strrep("0", new_decimal_pos - nchar(digits_text)))
      fractional_part <- ""
    } else if (new_decimal_pos <= 0L) {
      integer_part <- "0"
      fractional_part <- paste0(strrep("0", abs(new_decimal_pos)), digits_text)
    } else {
      integer_part <- substr(digits_text, 1L, new_decimal_pos)
      fractional_part <- substr(digits_text, new_decimal_pos + 1L, nchar(digits_text))
    }
    if (sign_char != "") {
      integer_part <- paste0(sign_char, integer_part)
    }

    fractional_part <- sub("0+$", "", fractional_part)
    list(integer_part = integer_part, fractional_part = fractional_part)
  }

  convert_parts_to_fraction <- function(integer_part, fractional_part) {
    numerator <- as.integer(paste0(integer_part, fractional_part))
    denominator <- 10.0^nchar(fractional_part)
    reduce_fraction(numerator, denominator)
  }

  reduce_fraction <- function(numerator, denominator) {
    if (abs(numerator) > .Machine[["integer.max"]] || denominator > .Machine[["integer.max"]]) {
      return(NULL)
    }
    numerator <- as.integer(numerator)
    denominator <- as.integer(denominator)
    if (denominator == 0L) {
      return(NULL)
    }
    divisor <- integer_gcd(abs(numerator), denominator)
    numerator <- numerator %/% divisor
    denominator <- denominator %/% divisor
    if (denominator == 1L) {
      return(NULL)
    }

    list(numerator = numerator, denominator = denominator)
  }

  integer_gcd <- function(a, b) {
    while (b != 0L) {
      tmp <- b
      b <- a %% b
      a <- tmp
    }
    abs(a)
  }

  stability_component_paths <- function(node) {
    vapply(xml_find_all_(node, "expr"), function(component) {
      repeat {
        inner <- xml_find_first_(component, "self::expr[*[1][self::OP-LEFT-PAREN]]/expr")
        if (is.na(inner)) break
        component <- inner
      }
      xml2::xml_path(component)
    }, character(1L))
  }

  stability_unwrap <- function(expr) {
    while (is.call(expr) && identical(expr[[1L]], as.name("(")) && length(expr) == 2L) {
      expr <- expr[[2L]]
    }
    expr
  }

  stability_call_name <- function(expr, package = "base") {
    expr <- stability_unwrap(expr)
    if (!is.call(expr)) return("")
    call_head <- expr[[1L]]
    if (is.symbol(call_head)) return(as.character(call_head))
    stability_namespace_name(call_head, package)
  }

  stability_namespace_name <- function(call_head, package) {
    if (!is.call(call_head) || length(call_head) != 3L) return("")
    if (!is.symbol(call_head[[1L]]) || !is.symbol(call_head[[3L]])) return("")
    if (!as.character(call_head[[1L]]) %in% c("::", ":::")) return("")
    if (!identical(call_head[[2L]], as.name(package))) return("")
    as.character(call_head[[3L]])
  }

  stability_match_call <- function(expr, name, package = "base") {
    expr <- stability_unwrap(expr)
    if (stability_call_name(expr, package) != name) return(NULL)
    actual <- as.list(expr)[-1L]
    invalid <- vapply(actual, identical, logical(1L), quote(expr = )) |
      vapply(actual, identical, logical(1L), quote(...))
    if (any(invalid)) return(NULL)
    definition <- get(name, envir = asNamespace(package), inherits = FALSE)
    if (is.primitive(definition)) {
      definition <- args(definition)
    }
    # Partially named arguments are supported even when their warnings are fatal.
    matched <- tryCatch(
      suppressWarnings(match.call(definition, expr, expand.dots = FALSE, envir = baseenv())),
      error = function(e) NULL
    )
    if (is.null(matched)) return(NULL)
    arguments <- as.list(matched)[-1L]
    required <- names(which(vapply(formals(definition), identical, logical(1L), quote(expr = ))))
    if (package == "stats") {
      # Central F/t distributions omit ncp; negative binomial calls supply prob OR mu.
      required <- setdiff(required, "ncp")
      if (endsWith(name, "nbinom") && any(c("prob", "mu") %in% names(arguments))) {
        required <- setdiff(required, c("prob", "mu"))
      }
    }
    if (!all(required %in% names(arguments))) return(NULL)
    arguments
  }

  stability_operator <- function(expr, operator, n = 2L) {
    expr <- stability_unwrap(expr)
    is.call(expr) && identical(expr[[1L]], as.name(operator)) && length(expr) == n + 1L
  }

  stability_one <- function(expr) {
    expr <- stability_unwrap(expr)
    if (stability_operator(expr, "+", 1L)) expr <- stability_unwrap(expr[[2L]])
    is.numeric(expr) && !is.complex(expr) && length(expr) == 1L && !is.na(expr) && expr == 1L
  }

  stability_offset <- function(expr, subtract = FALSE) {
    expr <- stability_unwrap(expr)
    if (stability_operator(expr, "+")) {
      if (stability_one(expr[[2L]])) return(expr[[3L]])
      if (stability_one(expr[[3L]])) return(expr[[2L]])
    }
    if (subtract && stability_operator(expr, "-") && stability_one(expr[[2L]])) {
      return(call("-", call("(", expr[[3L]])))
    }
    NULL
  }

  stability_has_complex <- function(expr) {
    if (is.complex(expr)) return(TRUE)
    if (!is.call(expr)) return(FALSE)
    if (stability_call_name(expr) %in% c("complex", "as.complex")) return(TRUE)
    parts <- as.list(expr)
    parts <- parts[!vapply(parts, identical, logical(1L), quote(expr = ))]
    any(vapply(parts, stability_has_complex, logical(1L)))
  }

  stability_text <- function(expr) deparse1(expr, width.cutoff = 500L)

  stability_suggest <- function(expr) paste0("Use ", stability_text(expr), " for better numerical stability.")

  stability_new_call <- function(name, arguments, package = NULL) {
    call_head <- if (is.null(package)) as.name(name) else call("::", as.name(package), as.name(name))
    as.call(c(list(call_head), arguments))
  }

  stability_flag <- function(arguments, name, default) {
    if (!name %in% names(arguments)) return(default)
    value <- stability_unwrap(arguments[[name]])
    if (identical(value, TRUE) || identical(value, FALSE)) return(value)
    NA
  }

  stability_probability <- function(expr, probability_names, logarithm = FALSE, complement = FALSE) {
    name <- stability_call_name(expr, "stats")
    if (!name %in% probability_names) return(NULL)
    arguments <- stability_match_call(expr, name, "stats")
    if (is.null(arguments) || !identical(stability_flag(arguments, "log.p", FALSE), FALSE)) return(NULL)
    if (complement) {
      lower <- stability_flag(arguments, "lower.tail", TRUE)
      if (is.na(lower)) return(NULL)
      arguments$lower.tail <- !lower
    }
    if (logarithm) arguments$log.p <- TRUE
    stability_new_call(name, arguments, "stats")
  }

  stability_complement <- function(expr) {
    expr <- stability_unwrap(expr)
    if (stability_operator(expr, "-") && stability_one(expr[[2L]])) return(expr[[3L]])
    NULL
  }

  stability_replacement <- function(expr, assume_real_numeric, include_heuristics,
                                    density_names, probability_names, quantile_names) {
    expr <- stability_unwrap(expr)
    log_args <- stability_match_call(expr, "log")
    # Even an explicitly supplied natural base is skipped: evaluating it may have side effects.
    if (!is.null(log_args) && !"base" %in% names(log_args)) {
      return(stability_log_replacement(
        stability_unwrap(log_args$x), assume_real_numeric, include_heuristics,
        density_names, probability_names
      ))
    }
    complement <- stability_complement(expr)
    if (!is.null(complement)) {
      replacement <- stability_probability(complement, probability_names, complement = TRUE)
      if (!is.null(replacement)) return(stability_suggest(replacement))
    }
    replacement <- stability_quantile(expr, quantile_names)
    if (!is.null(replacement)) return(stability_suggest(replacement))
    if (assume_real_numeric && !stability_has_complex(expr)) return(stability_real_replacement(expr))
    NULL
  }

  stability_quantile <- function(expr, quantile_names) {
    name <- stability_call_name(expr, "stats")
    if (!name %in% quantile_names) return(NULL)
    arguments <- stability_match_call(expr, name, "stats")
    if (is.null(arguments) || !identical(stability_flag(arguments, "log.p", FALSE), FALSE)) return(NULL)
    probability <- stability_complement(arguments$p)
    lower <- stability_flag(arguments, "lower.tail", TRUE)
    if (is.null(probability) || is.na(lower)) return(NULL)
    arguments$p <- probability
    arguments$lower.tail <- !lower
    stability_new_call(name, arguments, "stats")
  }

  stability_log_replacement <- function(expr, assume_real_numeric, include_heuristics,
                                        density_names, probability_names) {
    complement <- stability_complement(expr)
    replacement <- stability_probability(
      complement %||% expr, probability_names,
      logarithm = TRUE, complement = !is.null(complement)
    )
    if (!is.null(replacement)) return(stability_suggest(replacement))
    density_call <- stability_log_density(expr, density_names)
    if (!is.null(density_call)) return(stability_suggest(density_call))
    special <- stability_special_log(expr, include_heuristics)
    if (!is.null(special)) return(special)
    if (stability_assume_real(expr, assume_real_numeric)) {
      replacement <- stability_real_log(expr)
      if (!is.null(replacement)) return(stability_suggest(replacement))
    }
    if (include_heuristics && stability_log_sum_exp(expr)) {
      return(paste(
        "Consider a stable log-sum-exp implementation such as matrixStats::logSumExp();",
        "check empty inputs, missing values, and infinite values before replacing."
      ))
    }
    NULL
  }

  stability_assume_real <- function(expr, assume_real_numeric) {
    assume_real_numeric && !stability_has_complex(expr)
  }

  stability_log_density <- function(expr, density_names) {
    name <- stability_call_name(expr, "stats")
    if (!name %in% density_names) return(NULL)
    arguments <- stability_match_call(expr, name, "stats")
    if (is.null(arguments) || !identical(stability_flag(arguments, "log", FALSE), FALSE)) return(NULL)
    arguments$log <- TRUE
    stability_new_call(name, arguments, "stats")
  }

  stability_real_log <- function(expr) {
    shifted <- stability_offset(expr, subtract = TRUE)
    if (!is.null(shifted)) return(call("log1p", shifted))
    if (!stability_operator(expr, "/")) return(NULL)
    probability <- stability_unwrap(expr[[2L]])
    other <- stability_complement(expr[[3L]])
    if (stability_repeatable(probability) && identical(probability, stability_unwrap(other))) {
      return(stability_new_call("qlogis", list(probability), "stats"))
    }
    NULL
  }

  stability_special_log <- function(expr, include_heuristics) {
    absolute <- stability_match_call(expr, "abs")
    inner <- if (is.null(absolute)) expr else stability_unwrap(absolute$x)
    name <- stability_call_name(inner)
    replacements <- c(beta = "lbeta", gamma = "lgamma", factorial = "lfactorial", choose = "lchoose")
    if (!name %in% names(replacements)) return(NULL)
    arguments <- stability_match_call(inner, name)
    if (is.null(arguments)) return(NULL)
    conditional <- name != "beta" && is.null(absolute)
    if (conditional && !include_heuristics) return(NULL)
    replacement <- stability_new_call(replacements[[name]], arguments)
    if (conditional) {
      return(paste0(
        "Consider ", stability_text(replacement), " only where ", name,
        "() is non-negative; the replacement computes the logarithm of its absolute value."
      ))
    }
    stability_suggest(replacement)
  }

  stability_repeatable <- function(expr) {
    is.symbol(expr) || (is.numeric(expr) && !is.complex(expr) && length(expr) == 1L)
  }

  stability_real_replacement <- function(expr) {
    replacement <- stability_expm1(expr)
    if (!is.null(replacement)) return(stability_suggest(replacement))
    if (!stability_operator(expr, "/")) return(NULL)
    denominator <- stability_offset(expr[[3L]])
    arguments <- stability_match_call(denominator, "exp")
    if (is.null(arguments)) return(NULL)
    exponent <- stability_unwrap(arguments$x)
    if (stability_one(expr[[2L]])) {
      return(stability_suggest(stability_new_call("plogis", list(stability_negate(exponent)), "stats")))
    }
    numerator <- stability_match_call(expr[[2L]], "exp")
    if (is.null(numerator)) return(NULL)
    if (stability_same_value(numerator$x, exponent)) {
      return(stability_suggest(stability_new_call("plogis", list(exponent), "stats")))
    }
    NULL
  }

  stability_same_value <- function(left, right) {
    stability_repeatable(right) && identical(stability_unwrap(left), right)
  }

  stability_negate <- function(expr) {
    if (stability_operator(expr, "-", 1L)) return(expr[[2L]])
    call("-", call("(", expr))
  }

  stability_expm1 <- function(expr) {
    if (!stability_operator(expr, "-")) return(NULL)
    left <- stability_unwrap(expr[[2L]])
    right <- stability_unwrap(expr[[3L]])
    if (stability_one(right)) {
      arguments <- stability_match_call(left, "exp")
      if (!is.null(arguments)) return(call("expm1", arguments$x))
    }
    if (stability_one(left)) {
      arguments <- stability_match_call(right, "exp")
      if (!is.null(arguments)) return(call("-", call("expm1", arguments$x)))
    }
    NULL
  }

  stability_log_sum_exp <- function(expr) {
    if (stability_operator(expr, "+")) {
      return(!is.null(stability_match_call(expr[[2L]], "exp")) &&
               !is.null(stability_match_call(expr[[3L]], "exp")))
    }
    if (stability_call_name(expr) != "sum") return(FALSE)
    arguments <- as.list(expr)[-1L]
    if (length(arguments) != 1L || (!is.null(names(arguments)) && nzchar(names(arguments)[1L]))) return(FALSE)
    !is.null(stability_match_call(arguments[[1L]], "exp"))
  }

  check_decimal_fractions <- function(source_expression) {
    tracked_funs <- c("as.integer", "ceiling", "floor", "trunc")
    xml_calls <- source_expression$xml_find_function_calls(tracked_funs)
    if (length(xml_calls) == 0L) {
      return(list())
    }

    # Match as.integer calls where the argument is a simple binary expr with a
    # decimal literal (including 1e-3-style) so we can safely suggest integer
    # arithmetic replacements.
    # bad_expr narrows to as.integer(<binary>) so we can safely rewrite and
    # pinpoint the literal; more complex expressions are left alone.
    bad_expr <- xml_find_all_(
      xml_calls,
      "parent::expr[
        expr[2][
          (
            OP-STAR
            and expr//NUM_CONST
          ) or (
            OP-SLASH
            and expr[2]//NUM_CONST
          )
        ]
        and expr[2]//NUM_CONST[
          contains(text(), '.')
          or contains(translate(text(), 'E', 'e'), 'e-')
        ]
      ]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    bad_expr <- strip_comments_from_subtree(bad_expr)
    bad_expr <- xml_find_all_(
      bad_expr,
      "self::expr[expr[2][count(expr) = 2]]"
    )
    if (length(bad_expr) == 0L) {
      return(list())
    }

    # arg_expr is the argument expression to as.integer(<arg>).
    arg_expr <- xml_find_all_(bad_expr, "expr[2]")
    expr_children <- xml_find_all_(arg_expr, "./expr")
    is_numeric_child <- vapply(expr_children, function(expr) {
      !is.na(xml_find_first_(expr, ".//NUM_CONST"))
    }, logical(1L))
    num_expr <- xml_find_all_(expr_children[is_numeric_child], ".//NUM_CONST")
    # num_text is the literal string of the decimal constant we parse into a fraction.
    num_text <- xml_text(num_expr)
    other_expr <- expr_children[!is_numeric_child]
    # other_factor is the non-numeric operand (e.g., x or x + y) kept in the replacement.
    other_factor <- xml_text(other_expr)
    operator <- vapply(arg_expr, function(expr) {
      xml_name_(xml_find_first_(expr, "./OP-STAR | ./OP-SLASH"))
    }, character(1L))
    # Unary minus is its own node in the XML AST, so we need to inspect ancestors
    # to preserve sign in the replacement suggestion.
    is_negative <- vapply(num_expr, function(expr) {
      !is.na(xml_find_first_(expr, "ancestor::expr[OP-MINUS and count(expr) = 1]"))
    }, logical(1L))

    filtered <- filter_decimal_fraction_inputs(
      bad_expr = bad_expr,
      num_text = num_text,
      other_factor = other_factor,
      operator = operator,
      is_negative = is_negative
    )
    if (length(filtered[["bad_expr"]]) == 0L) {
      return(list())
    }

    fractions <- lapply(filtered[["num_text"]], function(number) {
      parts <- extract_decimal_parts(number)
      if (!lint_exact_binary && is_exact_binary_literal(parts[["fractional_part"]])) {
        return(NULL)
      }
      convert_parts_to_fraction(
        integer_part = parts[["integer_part"]],
        fractional_part = parts[["fractional_part"]]
      )
    })
    filtered <- filter_decimal_fractions(
      bad_expr = filtered[["bad_expr"]],
      other_factor = filtered[["other_factor"]],
      operator = filtered[["operator"]],
      is_negative = filtered[["is_negative"]],
      fractions = fractions
    )
    if (length(filtered[["bad_expr"]]) == 0L) {
      return(list())
    }

    call_names <- xp_call_name(filtered[["bad_expr"]], depth = 1L)
    lint_message <- build_decimal_fraction_message(
      fractions = filtered[["fractions"]],
      call_names = call_names,
      other_factor = filtered[["other_factor"]],
      operator = filtered[["operator"]],
      is_negative = filtered[["is_negative"]]
    )

    xml_nodes_to_lints(
      filtered[["bad_expr"]],
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning",
      column_number_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_start_xpath = "number(./expr[2]//NUM_CONST/@col1)",
      range_end_xpath = "number(./expr[2]//NUM_CONST/@col2)"
    )
  }

  check_numerical_stability <- function(source_expression) {
    candidates <- xml_find_all_(source_expression$xml_parsed_content, stability_xpath)
    covered_paths <- character()
    lints <- list()
    for (node in candidates) {
      # Report a combined replacement once, rather than also linting its parts.
      if (xml2::xml_path(node) %in% covered_paths) next
      expr <- xml2lang(node)
      lint_message <- stability_replacement(
        expr, assume_real_numeric, include_heuristics,
        density_names, probability_names, quantile_names
      )
      if (is.null(lint_message)) next
      covered_paths <- c(covered_paths, stability_component_paths(node))
      lints[[length(lints) + 1L]] <- xml_nodes_to_lints(
        node, source_expression, lint_message = lint_message, type = "warning"
      )
    }
    lints
  }

  Linter(linter_level = "expression", function(source_expression) {
    comparison_nodes <- xml_find_all_(source_expression$xml_parsed_content, comparison_xpath)
    c(
      check_decimal_fractions(source_expression),
      xml_nodes_to_lints(
        comparison_nodes,
        source_expression,
        lint_message = "Avoid equality comparisons with non-integer numerics; use abs(x - y) < eps instead.",
        type = "warning"
      ),
      check_numerical_stability(source_expression)
    )
  })
}
