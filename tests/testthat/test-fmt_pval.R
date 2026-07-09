test_that("fmt_pval: basic threshold behavior — value below threshold", {
  expect_equal(fmt_pval(0.03, alpha = "0.05"), "<0.05")
  expect_equal(fmt_pval(0.001, alpha = "0.05"), "<0.05")
  expect_equal(fmt_pval(0.0001, alpha = "0.05"), "<0.05")
})

test_that("fmt_pval: basic threshold behavior — value at or above threshold", {
  # Exactly at threshold: NOT below, so should be rounded and displayed
  expect_equal(fmt_pval(0.05, alpha = "0.05"), "0.05")
  expect_equal(fmt_pval(0.055, alpha = "0.05"), "0.06")
  expect_equal(fmt_pval(0.10, alpha = "0.05"), "0.10")
})

test_that("fmt_pval: threshold label appears verbatim in output", {
  expect_equal(fmt_pval(0.04, alpha = "0.10"), "<0.10")
  expect_equal(fmt_pval(0.0005, alpha = "0.001"), "<0.001")
  expect_equal(fmt_pval(0.04, alpha = "0.05"), "<0.05")
})

test_that("fmt_pval: space argument controls whitespace after '<'", {
  expect_equal(fmt_pval(0.03, alpha = "0.05", space = TRUE),  "< 0.05")
  expect_equal(fmt_pval(0.03, alpha = "0.05", space = FALSE), "<0.05")
})

test_that("fmt_pval: n argument controls decimal places for displayed values", {
  expect_equal(fmt_pval(0.051, n = 3, alpha = "0.05"), "0.051")
  expect_equal(fmt_pval(0.051, n = 2, alpha = "0.05"), "0.05")
  expect_equal(fmt_pval(0.999, n = 2, alpha = "0.05"), "1.00")
  expect_equal(fmt_pval(0.5555, n = 3, alpha = "0.05"), "0.556")
})

test_that("fmt_pval: NA values are preserved as character 'NA'", {
  expect_equal(fmt_pval(NA_real_, alpha = "0.05"), "NA")
  expect_equal(
    fmt_pval(c(0.051, NA), n = 2, alpha = "0.05"),
    c("0.05", "NA")
  )
})

test_that("fmt_pval: vectorized input — mixed above and below threshold", {
  result <- fmt_pval(c(0.051, 0.001), n = 3, alpha = "0.05")
  expect_equal(result, c("0.051", "<0.05"))
})

test_that("fmt_pval: vectorized input — mixed above and below a small threshold", {
  result <- fmt_pval(c(0.051, 0.0008), n = 2, alpha = "0.001")
  expect_equal(result, c("0.05", "<0.001"))
})

test_that("fmt_pval: vectorized alpha — per-element thresholds", {
  result <- fmt_pval(c(0.051, 0.001), n = 3, alpha = c("0.05", "0.10"))
  expect_equal(result, c("0.051", "<0.10"))
})

test_that("fmt_pval: alpha with numeric input", {
  expect_equal(fmt_pval(0.08, 2, "0.10"), "<0.10")
  expect_equal(fmt_pval(0.08, 2, 0.10), "<0.1")
})
# --- Input validation errors ---

test_that("fmt_pval: errors when x is not numeric", {
  expect_error(fmt_pval("0.05", alpha = "0.05"), "x must be numeric")
})

test_that("fmt_pval: errors when x is out of [0, 1] range", {
  expect_error(fmt_pval(1.5, alpha = "0.05"), "x must be between 0 and 1")
  expect_error(fmt_pval(-0.1, alpha = "0.05"), "x must be between 0 and 1")
})

test_that("fmt_pval: errors when n is negative or non-integer", {
  expect_error(fmt_pval(0.05, n = -1, alpha = "0.05"), "n must be a non-negative integer")
  expect_error(fmt_pval(0.05, n = 1.5, alpha = "0.05"), "n must be a non-negative integer")
})

test_that("fmt_pval: errors when n length mismatches x length", {
  expect_error(
    fmt_pval(c(0.05, 0.10), n = c(2L, 3L, 4L), alpha = "0.05"),
    "n must be scalar or same length as x"
  )
})

test_that("fmt_pval: errors when alpha is not coercible to numeric", {
  expect_error(fmt_pval(0.05, alpha = "abc"), "alpha must be numeric when coerced")
})

test_that("fmt_pval: errors when alpha is outside (0, 1)", {
  expect_error(fmt_pval(0.05, alpha = "0"),   "alpha must be between 0 and 1")
  expect_error(fmt_pval(0.05, alpha = "1"),   "alpha must be between 0 and 1")
  expect_error(fmt_pval(0.05, alpha = "1.5"), "alpha must be between 0 and 1")
})

test_that("fmt_pval: errors when alpha length mismatches x length", {
  expect_error(
    fmt_pval(c(0.05, 0.10), alpha = c("0.05", "0.10", "0.05")),
    "alpha must be scalar or same length as x"
  )
})

test_that("fmt_pval: errors when space is not a logical scalar", {
  expect_error(fmt_pval(0.05, alpha = "0.05", space = "yes"), "space must be logical")
  expect_error(fmt_pval(0.05, alpha = "0.05", space = NA),    "space must be logical")
})