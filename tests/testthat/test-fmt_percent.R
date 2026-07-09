# --- basic functionality ---
test_that("fmt_percent() formats value and percentage with default precision", {
  expect_equal(fmt_percent(15, 100), "15 (15%)")
})

test_that("fmt_percent() respects n = 2 for both value and percentage", {
  expect_equal(fmt_percent(15, 100, n = 2), "15.00 (15.00%)")
})

test_that("fmt_percent() respects independent perc_n when n = 0", {
  expect_equal(fmt_percent(15, 100, n = 0, perc_n = 2), "15 (15.00%)")
})

# --- default denom = sum(x) ---
test_that("fmt_percent() defaults denom to sum(x) for vectorized input", {
  expect_equal(
    fmt_percent(c(30, 50, 20)),
    c("30 (30%)", "50 (50%)", "20 (20%)")
  )
})

# --- symbol ---
test_that("fmt_percent() omits percentage symbol when symbol = ''", {
  expect_equal(
    fmt_percent(c(30, 50, 20), symbol = ""),
    c("30 (30)", "50 (50)", "20 (20)")
  )
})

test_that("fmt_percent() uses custom symbol in output", {
  expect_equal(fmt_percent(15, 100, symbol = " pct"), "15 (15 pct)")
})

# --- rounding ---
test_that("fmt_percent() rounds percentage half-up at perc_n = 1", {
  expect_equal(fmt_percent(1, 3, perc_n = 1), "1 (33.3%)")
  expect_equal(fmt_percent(2, 3, perc_n = 1), "2 (66.7%)")
})

# --- trailing zeros ---
test_that("fmt_percent() preserves trailing zeros to requested precision", {
  expect_equal(fmt_percent(10, 100, n = 2), "10.00 (10.00%)")
})

# --- scalar denom as shared denominator ---
test_that("fmt_percent() applies scalar denom as denominator across all elements", {
  expect_equal(
    fmt_percent(c(30, 50, 20), denom = 200),
    c("30 (15%)", "50 (25%)", "20 (10%)")
  )
})

# --- input validation ---
test_that("fmt_percent() rejects non-numeric x", {
  expect_error(fmt_percent("a", 100), "x must be numeric")
})

test_that("fmt_percent() rejects NA denom", {
  expect_error(fmt_percent(15, NA_real_), "denom must be numeric and non-missing")
})

test_that("fmt_percent() rejects negative n", {
  expect_error(fmt_percent(15, 100, n = -1), "n must be a non-negative integer")
})

test_that("fmt_percent() rejects non-integer n", {
  expect_error(fmt_percent(15, 100, n = 1.5), "n must be a non-negative integer")
})

test_that("fmt_percent() rejects negative perc_n", {
  expect_error(fmt_percent(15, 100, perc_n = -1), "perc_n must be a non-negative integer")
})

test_that("fmt_percent() rejects non-character symbol", {
  expect_error(fmt_percent(15, 100, symbol = 1), "symbol must be a character string")
})

test_that("fmt_percent() rejects vector symbol", {
  expect_error(fmt_percent(15, 100, symbol = c("%", "%%")), "symbol must be a character string")
})

test_that("fmt_percent() rejects non-logical show_lt", {
  expect_error(fmt_percent(15, 100, show_lt = "yes"), "show_lt must be a logical scalar")
  expect_error(fmt_percent(15, 100, show_lt = NA),    "show_lt must be a logical scalar")
})

# --- show_lt = TRUE (default) ---
test_that("fmt_percent() replaces rounded-zero percentage with '<' threshold for nonzero x", {
  expect_equal(fmt_percent(1L, 100000L, perc_n = 0), "1 (<1%)")
  expect_equal(fmt_percent(1L, 100000L, perc_n = 1), "1 (<0.1%)")
  expect_equal(fmt_percent(1L, 100000L, perc_n = 2), "1 (<0.01%)")
  expect_equal(fmt_percent(1L, 100000L, perc_n = 3), "1 (0.001%)")
})

test_that("fmt_percent() reports '0%' when x is exactly zero", {
  expect_equal(fmt_percent(0L, 1000L), "0 (0%)")
})

test_that("fmt_percent() keeps the rounded-zero percentage when show_lt = FALSE", {
  expect_equal(fmt_percent(1L, 100000L, perc_n = 2, show_lt = FALSE), "1 (0.00%)")
})

test_that("fmt_percent: NA elements propagate as 'NA (NA%)'", {
  expect_equal(fmt_percent(NA_real_, 100), "NA (NA%)")
  expect_equal(fmt_percent(c(5, NA)), c("5 (100%)", "NA (NA%)"))
})

test_that("fmt_percent: negative values are never '<' substituted and never '-0.00'", {
  expect_equal(fmt_percent(-1, 100000, perc_n = 2), "-1 (0.00%)")
})

# --- zero denominator ---
test_that("fmt_percent() displays zero_denom when the denominator is zero", {
  expect_equal(fmt_percent(15, 0),    "15 (-)")
  expect_equal(fmt_percent(c(0, 0)),  c("0 (-)", "0 (-)"))
  expect_equal(fmt_percent(NA_real_), "NA (-)")
})

test_that("fmt_percent() applies zero_denom element-wise for vector denom", {
  expect_equal(fmt_percent(c(1, 2), denom = c(0, 4)), c("1 (-)", "2 (50%)"))
})

test_that("fmt_percent() respects custom zero_denom", {
  expect_equal(fmt_percent(0, 0, zero_denom = "NA"), "0 (NA)")
})

test_that("fmt_percent() rejects non-character or NA zero_denom", {
  expect_error(fmt_percent(15, 100, zero_denom = 1),  "zero_denom must be a character string")
  expect_error(fmt_percent(15, 100, zero_denom = NA), "zero_denom must be a character string")
})
