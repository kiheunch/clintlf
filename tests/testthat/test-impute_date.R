# --- ymd (default: last month, last day) ---
test_that("impute_date() imputes partial ymd dates correctly", {
  expect_equal(impute_date(c("2025-07-19", "2025-07", "2025-7", "2025")),
               c("2025-07-19", "2025-07-31", "2025-07-31", "2025-12-31"))
})

test_that("impute_date() pads single-digit month and day in ymd format", {
  expect_equal(impute_date("2025-7-9"), "2025-07-09")
})

# --- default last-day fill is month-aware and leap-year aware ---
test_that("impute_date() default fill_day resolves the last day of the month", {
  expect_equal(impute_date(c("2025-02", "2025-04", "2025-12")),
               c("2025-02-28", "2025-04-30", "2025-12-31"))
  expect_equal(impute_date("2024-02"), "2024-02-29")  # leap year
})

# --- NA passthrough ---
test_that("impute_date() passes through NA values", {
  expect_equal(impute_date(c("2025-07", NA)), c("2025-07-31", NA_character_))
  expect_equal(impute_date(NA), NA_character_)
})

# --- custom fill ---
test_that("impute_date() respects custom fill_month and fill_day values", {
  expect_equal(impute_date("2025", fill_month = "12", fill_day = "31"), "2025-12-31")
  expect_equal(impute_date("2025", fill_month = "01", fill_day = "01"), "2025-01-01")
  expect_equal(impute_date("2025", fill_day = "15"),                    "2025-12-15")
})

test_that("impute_date() pads single-digit custom fill values", {
  expect_equal(impute_date("2025", fill_month = "6", fill_day = "1"), "2025-06-01")
})

# --- separator ---
test_that("impute_date() handles multiple separators correctly", {
  expect_equal(impute_date(c("2025-07-19", "2025/07/19", "2025.07.19")),
               c("2025-07-19", "2025-07-19", "2025-07-19"))
})

test_that("impute_date() handles custom sep override correctly", {
  expect_equal(impute_date(c("2025_07_19", "2025_07", "2025"), sep = "_"),
               c("2025-07-19", "2025-07-31", "2025-12-31"))
})

# --- dmy format ---
test_that("impute_date() imputes partial dmy dates correctly", {
  expect_equal(impute_date(c("19-07-2025", "07-2025", "2025"), fmt = "dmy"),
               c("2025-07-19", "2025-07-31", "2025-12-31"))
})

test_that("impute_date() pads single-digit month and day in dmy format", {
  expect_equal(impute_date("9-7-2025", fmt = "dmy"), "2025-07-09")
})

# --- mdy format ---
test_that("impute_date() imputes partial mdy dates correctly", {
  expect_equal(impute_date(c("07-19-2025", "07-2025", "2025"), fmt = "mdy"),
               c("2025-07-19", "2025-07-31", "2025-12-31"))
})

test_that("impute_date() pads single-digit month and day in mdy format", {
  expect_equal(impute_date("7-9-2025", fmt = "mdy"), "2025-07-09")
})

# --- input validation errors ---
test_that("impute_date() throws informative errors on invalid inputs", {
  expect_error(impute_date(20250101),                    "x must be a character vector")
  expect_error(impute_date("2025", fill_month = "13"),   "fill_month must be a valid month")
  expect_error(impute_date("2025", fill_day   = "32"),   "fill_day must be a valid day")
  expect_error(impute_date("2025", sep = 1),             "sep must be a single character string")
  expect_error(impute_date("2025", fmt = "ydm"),         "should be one of")
})

# --- output warnings ---
test_that("impute_date() warns when parsing introduces new NAs not present in input", {
  expect_warning(
    impute_date(c("2025-07-19", "   ", NA), sep = "-"),
    regexp = "non-missing but produced NA.*indices: 2"
  )
})

test_that("impute_date() warns when output elements do not conform to yyyy-mm-dd format", {
  expect_warning(
    impute_date(c("2025-07-19", "not a date"), sep = "-"),
    regexp = "do not conform to yyyy-mm-dd.*indices: 2"
  )
})
test_that("impute_date() warns on out-of-range month or day components", {
  expect_warning(impute_date("2025-13-45"), "out-of-range month or day")
})

test_that("impute_date() returns NA with warning for more than 3 date components", {
  expect_warning(out <- impute_date("2025-07-19-05"),
                 "non-missing but produced NA.*indices: 1")
  expect_identical(out, NA_character_)
})
