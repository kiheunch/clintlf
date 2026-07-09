# ── fmt_ci ────────────────────────────────────────────────────────────────────

# --- standard numeric input --------------------------------------------------

test_that("fmt_ci: basic numeric input with default decimal places", {
  expect_equal(fmt_ci(3.14159, 1.23456, 5.67890), "3.14 (1.23, 5.68)")
})

test_that("fmt_ci: independent decimal control for estimate and CI", {
  expect_equal(fmt_ci(3.14159, 1.23456, 5.67890, est_n = 2, ci_n = 1), "3.14 (1.2, 5.7)")
})

test_that("fmt_ci: vectorized numeric input", {
  expect_equal(
    fmt_ci(c(1.111, 2.222), c(0.500, 1.500), c(1.700, 2.900)),
    c("1.11 (0.50, 1.70)", "2.22 (1.50, 2.90)")
  )
})

test_that("fmt_ci: trailing zeros preserved", {
  expect_equal(fmt_ci(1.1, 0.5, 2.0), "1.10 (0.50, 2.00)")
})

test_that("fmt_ci: half-up rounding behavior", {
  expect_equal(fmt_ci(2.5, 1.5, 3.5, est_n = 0), "3 (2, 4)")
})

test_that("fmt_ci: negative numeric values", {
  expect_equal(fmt_ci(-2.111, -2.225, -0.500), "-2.11 (-2.23, -0.50)")
})

test_that("fmt_ci: est_n = 0 produces integer-like strings", {
  expect_equal(fmt_ci(3.7, 1.2, 5.8, est_n = 0), "4 (1, 6)")
})

# --- character input ----------------------------------------------------------

test_that("fmt_ci: character numeric strings are coerced and rounded", {
  expect_equal(fmt_ci("3.14159", "1.23456", "5.67890"), "3.14 (1.23, 5.68)")
})

test_that("fmt_ci: character input with decimal control", {
  expect_equal(fmt_ci("3.14159", "1.23456", "5.67890", est_n = 2, ci_n = 1), "3.14 (1.2, 5.7)")
})

test_that("fmt_ci: vectorized character input", {
  expect_equal(
    fmt_ci(c("1.111", "2.222"), c("0.500", "1.500"), c("1.700", "2.900")),
    c("1.11 (0.50, 1.70)", "2.22 (1.50, 2.90)")
  )
})

test_that("fmt_ci: non-coercible character input throws error for est", {
  expect_error(fmt_ci("abc", "1.0", "2.0"), "'est' contains values that cannot be coerced to numeric")
})

test_that("fmt_ci: non-coercible character input throws error for lci", {
  expect_error(fmt_ci("1.0", "abc", "2.0"), "'lci' contains values that cannot be coerced to numeric")
})

test_that("fmt_ci: non-coercible character input throws error for uci", {
  expect_error(fmt_ci("1.0", "2.0", "abc"), "'uci' contains values that cannot be coerced to numeric")
})

# --- mixed numeric and character input ---------------------------------------

test_that("fmt_ci: mixed numeric est and character lci, uci", {
  expect_equal(fmt_ci(3.14159, "1.23456", "5.67890"), "3.14 (1.23, 5.68)")
})

test_that("fmt_ci: mixed character est and numeric lci, uci", {
  expect_equal(fmt_ci("3.14159", 1.23456, 5.67890), "3.14 (1.23, 5.68)")
})

test_that("fmt_ci: mixed types vectorized", {
  expect_equal(
    fmt_ci(c(1.111, 2.222), c("0.500", "1.500"), c(1.700, 2.900)),
    c("1.11 (0.50, 1.70)", "2.22 (1.50, 2.90)")
  )
})

# --- NA input ----------------------------------------------------------------

test_that("fmt_ci: NA_real_ in all inputs produces NA (NA, NA)", {
  expect_equal(fmt_ci(NA_real_, NA_real_, NA_real_), "NA (NA, NA)")
})

test_that("fmt_ci: bare NA in all inputs produces NA (NA, NA)", {
  expect_equal(fmt_ci(NA, NA, NA), "NA (NA, NA)")
})

test_that("fmt_ci: NA_character_ in all inputs produces NA (NA, NA)", {
  expect_equal(fmt_ci(NA_character_, NA_character_, NA_character_), "NA (NA, NA)")
})

test_that("fmt_ci: partial NA_real_ produces mixed output", {
  expect_equal(fmt_ci(0.2, NA_real_, 0.5), "0.20 (NA, 0.50)")
})

test_that("fmt_ci: partial NA_character_ produces mixed output", {
  expect_equal(fmt_ci(0.2, NA_character_, 0.5), "0.20 (NA, 0.50)")
})

test_that("fmt_ci: vectorized input with NA element", {
  expect_equal(
    fmt_ci(c(1.111, NA_real_), c(0.500, NA_real_), c(1.700, NA_real_)),
    c("1.11 (0.50, 1.70)", "NA (NA, NA)")
  )
})

test_that("fmt_ci: mixed NA and character in same vector", {
  expect_equal(
    fmt_ci(c("1.111", NA_character_), c("0.500", NA_character_), c("1.700", NA_character_)),
    c("1.11 (0.50, 1.70)", "NA (NA, NA)")
  )
})

# --- input validation --------------------------------------------------------

test_that("fmt_ci: error on non-numeric non-character est", {
  expect_error(fmt_ci(TRUE, 1.0, 2.0), "'est' must be numeric or character")
})

test_that("fmt_ci: error on mismatched lengths", {
  expect_error(fmt_ci(c(1, 2), 1, c(2, 3)), "est, lci, uci must be the same length")
})

test_that("fmt_ci: error on empty input", {
  expect_error(fmt_ci(numeric(0), numeric(0), numeric(0)), "est, lci, uci must not be empty")
})

test_that("fmt_ci: error on non-integer est_n", {
  expect_error(fmt_ci(1.5, 0.5, 2.5, est_n = 1.5), "est_n must be a non-negative integer scalar")
})

test_that("fmt_ci: error on negative ci_n", {
  expect_error(fmt_ci(1.5, 0.5, 2.5, ci_n = -1), "ci_n must be a non-negative integer scalar")
})

test_that("fmt_ci: error on NA est_n", {
  expect_error(fmt_ci(1.5, 0.5, 2.5, est_n = NA_integer_), "est_n must be a non-negative integer scalar")
})

test_that("fmt_ci: eps is forwarded to fmt_round via ...", {
  expect_equal(fmt_ci(2436.845, 1, 3, est_n = 2, ci_n = 0, eps = TRUE),  "2436.85 (1, 3)")
  expect_equal(fmt_ci(2436.845, 1, 3, est_n = 2, ci_n = 0, eps = FALSE), "2436.84 (1, 3)")
})
