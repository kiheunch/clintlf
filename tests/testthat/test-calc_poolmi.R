# Reference values below were computed by an independent transcription of the
# Rubin (1987) and Barnard-Rubin (1999) formulas, not by calc_poolmi itself.

ref_est <- c(-2.57, -2.11, -1.85, -2.07, -2.13)
ref_se  <- c( 1.05,  0.91,  0.92,  0.85,  0.90)

test_that("calc_poolmi: vector interface matches Rubin (1987) reference values", {
  out <- calc_poolmi(est = ref_est, se = ref_se, df = "1987")

  expect_equal(out$pool.est, -2.146,        tolerance = 1e-8)
  expect_equal(out$pool.se,   0.9717592294, tolerance = 1e-8)
  expect_equal(out$lower,    -4.055012922,  tolerance = 1e-8)
  expect_equal(out$upper,    -0.2369870782, tolerance = 1e-8)
  expect_equal(out$p.val,     0.02765001698, tolerance = 1e-8)
})

test_that("calc_poolmi: vector interface matches Barnard-Rubin (1999) reference values", {
  out <- calc_poolmi(est = ref_est, se = ref_se, df = "1999", n = 100, k = 2)

  expect_equal(out$lower, -4.081787969,  tolerance = 1e-8)
  expect_equal(out$upper, -0.2102120307, tolerance = 1e-8)
  expect_equal(out$p.val,  0.03026888778, tolerance = 1e-8)
})

test_that("calc_poolmi: data frame interface matches vector interface", {
  d <- data.frame(IMPNO = 1:5, est = ref_est, SE = ref_se)

  expect_equal(
    calc_poolmi(est = "est", se = "SE", data = d),
    calc_poolmi(est = ref_est, se = ref_se)
  )
})

test_that("calc_poolmi: zero between-imputation variance returns finite results", {
  # identical estimates across imputations: lambda is clamped at 1e-04 (as in
  # mice::pool()), so df stays finite and no NaN is produced
  out_1999 <- calc_poolmi(est = rep(1.5, 5), se = rep(0.3, 5),
                          df = "1999", n = 100, k = 2)
  out_1987 <- calc_poolmi(est = rep(1.5, 5), se = rep(0.3, 5), df = "1987")

  expect_true(all(is.finite(unlist(out_1999))))
  expect_true(all(is.finite(unlist(out_1987))))
  expect_equal(out_1999$pool.est, 1.5)
  expect_equal(out_1999$pool.se,  0.3)
})

# --- input validation ---

test_that("calc_poolmi: errors on missing columns when data is supplied", {
  d <- data.frame(a = 1:5, b = 1:5)
  expect_error(calc_poolmi(est = "est", se = "SE", data = d), "not found in data")
})

test_that("calc_poolmi: errors on non-character est/se when data is supplied", {
  d <- data.frame(est = ref_est, SE = ref_se)
  expect_error(calc_poolmi(est = ref_est, se = "SE", data = d),
               "single column name")
})

test_that("calc_poolmi: errors on non-numeric vectors without data", {
  expect_error(calc_poolmi(est = "est", se = "SE"), "numeric vector")
})

test_that("calc_poolmi: errors on length mismatch", {
  expect_error(calc_poolmi(est = c(1, 2, 3), se = c(0.1, 0.2)), "same length")
})

test_that("calc_poolmi: errors with fewer than 2 imputations", {
  expect_error(calc_poolmi(est = 1.5, se = 0.3), "at least 2 imputations")
})

test_that("calc_poolmi: errors on NA or non-positive standard errors", {
  expect_error(calc_poolmi(est = c(1, 2), se = c(0.1, NA)), "NA")
  expect_error(calc_poolmi(est = c(1, 2), se = c(0.1, 0)),  "non-positive")
})

test_that("calc_poolmi: errors when n or k missing for df = '1999'", {
  expect_error(calc_poolmi(est = ref_est, se = ref_se, df = "1999"),
               "n and k must be provided")
})

test_that("calc_poolmi: errors when k >= n for df = '1999'", {
  expect_error(calc_poolmi(est = ref_est, se = ref_se, df = "1999", n = 2, k = 2),
               "k must be less than n")
})

test_that("calc_poolmi: errors on non-scalar conf, n, or k", {
  expect_error(calc_poolmi(est = ref_est, se = ref_se, conf = c(0.90, 0.95)),
               "conf must be a single value")
  expect_error(calc_poolmi(est = ref_est, se = ref_se, df = "1999",
                           n = c(100, 50), k = 2),
               "n must be a positive integer scalar")
  expect_error(calc_poolmi(est = ref_est, se = ref_se, df = "1999",
                           n = 100, k = c(2, 3)),
               "k must be a positive integer scalar")
})

# --- live cross-check against mice (skipped when not installed) ---

test_that("calc_poolmi matches mice::pool() using Barnard-Rubin (1999) degrees of freedom", {
  skip_if_not_installed("mice")
  imp <- mice::mice(mice::nhanes, m = 5, print = FALSE, seed = 18210)
  fit <- with(data = imp, lm(bmi ~ age))

  pooled_summary <- summary(mice::pool(fit), conf.int = TRUE)

  result_mice <- data.frame(
    pool.est = pooled_summary$estimate[2],
    pool.se  = pooled_summary$std.error[2],
    lower    = pooled_summary$`2.5 %`[2],
    upper    = pooled_summary$`97.5 %`[2],
    p.val    = pooled_summary$p.value[2]
  )

  test_data <- do.call(rbind, lapply(fit$analyses, function(model) {
    coefs <- summary(model)$coefficients
    data.frame(
      est = coefs["age", "Estimate"],
      SE  = coefs["age", "Std. Error"]
    )
  }))

  result_fmt <- calc_poolmi(
    est  = "est",
    se   = "SE",
    data = test_data,
    df   = "1999",
    n    = nrow(mice::nhanes),                # 25
    k    = length(coef(fit$analyses[[1]]))    # 2: intercept + age
  )

  expect_equal(result_mice, result_fmt, tolerance = 1e-5)
})
