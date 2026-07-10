# ── fmt_stat ──────────────────────────────────────────────────────────────────

# --- typical cells -------------------------------------------------------------

test_that("fmt_stat: mean (SD) cell", {
  expect_equal(fmt_stat(23.456, 4.5678, inner_n = 2), "23.5 (4.57)")
})

test_that("fmt_stat: median (Q1, Q3) cell", {
  expect_equal(fmt_stat(5.678, 2.345, 8.901), "5.7 (2.3, 8.9)")
})

test_that("fmt_stat: estimate with confidence bounds", {
  expect_equal(fmt_stat(3.14159, 1.23456, 5.67890, n = 2, inner_n = 2), "3.14 (1.23, 5.68)")
})

test_that("fmt_stat: min, max cell with no lead and no brackets", {
  expect_equal(fmt_stat(NULL, 0.123, 9.876, bracket = ""), "0.1, 9.9")
})

test_that("fmt_stat: square brackets", {
  expect_equal(fmt_stat(1.5, 0.5, 2.5, bracket = "["), "1.5 [0.5, 2.5]")
})

test_that("fmt_stat: custom separator", {
  expect_equal(fmt_stat(NULL, 1, 9, bracket = "", sep = " - ", inner_n = 0), "1 - 9")
})

test_that("fmt_stat: vectorized over rows", {
  expect_equal(
    fmt_stat(c(1.111, 2.222), c(0.500, 1.500), c(1.700, 2.900), n = 2, inner_n = 2),
    c("1.11 (0.50, 1.70)", "2.22 (1.50, 2.90)")
  )
})

# --- decimal control -----------------------------------------------------------

test_that("fmt_stat: inner_n defaults to 1 independent of n", {
  expect_equal(fmt_stat(1.111, 2.222, n = 2), "1.11 (2.2)")
})

test_that("fmt_stat: trailing zeros preserved", {
  expect_equal(fmt_stat(1.1, 0.5, 2.0, n = 2, inner_n = 2), "1.10 (0.50, 2.00)")
})

test_that("fmt_stat: rounds half away from zero", {
  expect_equal(fmt_stat(2.5, 1.5, 3.5, n = 0, inner_n = 0), "3 (2, 4)")
})

test_that("fmt_stat: negative values", {
  expect_equal(fmt_stat(-2.111, -2.225, -0.500, n = 2, inner_n = 2), "-2.11 (-2.23, -0.50)")
})

test_that("fmt_stat: eps is forwarded to fmt_round", {
  expect_equal(fmt_stat(2436.845, 1, 3, n = 2, inner_n = 0, eps = TRUE),  "2436.85 (1, 3)")
  expect_equal(fmt_stat(2436.845, 1, 3, n = 2, inner_n = 0, eps = FALSE), "2436.84 (1, 3)")
})

# --- character input -----------------------------------------------------------

test_that("fmt_stat: character numeric strings are coerced", {
  expect_equal(fmt_stat("3.14159", "1.23456", "5.67890", n = 2, inner_n = 2), "3.14 (1.23, 5.68)")
})

test_that("fmt_stat: mixed numeric and character input", {
  expect_equal(fmt_stat(3.14159, "1.23456", 5.67890, n = 2, inner_n = 2), "3.14 (1.23, 5.68)")
})

test_that("fmt_stat: non-coercible character throws for x", {
  expect_error(fmt_stat("abc", "1.0"), "'x' contains values that cannot be coerced to numeric")
})

test_that("fmt_stat: non-coercible character throws for a bracketed vector", {
  expect_error(fmt_stat("1.0", "1.0", "abc"), "vector 2 in \\.\\.\\. contains values that cannot be coerced to numeric")
})

# --- missing values ------------------------------------------------------------

test_that("fmt_stat: NA formats as NA by default", {
  expect_equal(fmt_stat(NA, NA, NA), "NA (NA, NA)")
  expect_equal(fmt_stat(0.2, NA_real_, 0.5, n = 2, inner_n = 2), "0.20 (NA, 0.50)")
  expect_equal(fmt_stat(NA_character_, NA_character_), "NA (NA)")
})

test_that("fmt_stat: vectorized input with NA element", {
  expect_equal(
    fmt_stat(c(1.111, NA), c(0.500, NA), c(1.700, NA), n = 2, inner_n = 2),
    c("1.11 (0.50, 1.70)", "NA (NA, NA)")
  )
})

test_that("fmt_stat: na text replaces NA and NaN", {
  expect_equal(fmt_stat(c(12.3, 15.8), c(4.56, NA), na = "-"), c("12.3 (4.6)", "15.8 (-)"))
  expect_equal(fmt_stat(NaN, 1, na = "-"), "- (1.0)")
})

# --- input validation ----------------------------------------------------------

test_that("fmt_stat: error without bracketed values", {
  expect_error(fmt_stat(1), "supply at least one bracketed value")
})

test_that("fmt_stat: named vectors in ... are caught", {
  expect_error(fmt_stat(1, 2, brackt = "["), "must be unnamed")
})

test_that("fmt_stat: error on non-numeric non-character input", {
  expect_error(fmt_stat(TRUE, 1), "'x' must be numeric or character")
})

test_that("fmt_stat: error on mismatched lengths", {
  expect_error(fmt_stat(c(1, 2), 1), "same length")
})

test_that("fmt_stat: zero-length input gives zero-length output", {
  expect_equal(fmt_stat(numeric(0), numeric(0)), character(0))
  expect_equal(fmt_stat(NULL, numeric(0)), character(0))
})

test_that("fmt_stat: error on bad n or inner_n", {
  expect_error(fmt_stat(1, 2, n = 1.5), "n must be a non-negative integer scalar")
  expect_error(fmt_stat(1, 2, inner_n = -1), "inner_n must be a non-negative integer scalar")
})

test_that("fmt_stat: error on unknown bracket", {
  expect_error(fmt_stat(1, 2, bracket = "{"), "bracket must be")
})
