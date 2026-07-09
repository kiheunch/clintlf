# tests/testthat/test-dict_label.R

test_that("dict_label() returns correct structure for a single data frame", {
  # Use airquality; assign labels explicitly to a subset of columns
  df <- airquality[1:5, c("Ozone", "Solar.R", "Wind")]
  attr(df$Ozone,   "label") <- "Ozone (ppb)"
  attr(df$Solar.R, "label") <- "Solar Radiation (lang)"
  # Wind intentionally left without a label

  result <- dict_label(df)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("varcode", "label"))
  expect_equal(nrow(result), ncol(df))
  expect_type(result$varcode, "character")
  expect_type(result$label,   "character")
})

test_that("dict_label() correctly extracts labels and fills '' for unlabelled columns", {
  df <- airquality[1:5, c("Ozone", "Solar.R", "Wind")]
  attr(df$Ozone,   "label") <- "Ozone (ppb)"
  attr(df$Solar.R, "label") <- "Solar Radiation (lang)"
  # Wind left unlabelled

  result <- dict_label(df)

  expect_equal(result$varcode, c("Ozone", "Solar.R", "Wind"))
  expect_equal(result$label,   c("Ozone (ppb)", "Solar Radiation (lang)", ""))
})

test_that("dict_label() returns '' for all labels when no column has a label attribute", {
  df <- mtcars[1:3, c("mpg", "cyl", "disp")]
  # No labels assigned

  result <- dict_label(df)

  expect_equal(result$label, c("", "", ""))
})

test_that("dict_label() returns a single data frame (not a list) for single df input", {
  df <- airquality[1:3, c("Ozone", "Wind")]
  attr(df$Ozone, "label") <- "Ozone (ppb)"

  result <- dict_label(df)

  expect_s3_class(result, "data.frame")
  expect_false(is.list(result) && !is.data.frame(result))
})

# ---------------------------------------------------------------------------
# Vectorized (named list of data frames)
# ---------------------------------------------------------------------------

test_that("dict_label() returns a named list for a named list of data frames", {
  df1 <- airquality[1:3, c("Ozone", "Wind")]
  attr(df1$Ozone, "label") <- "Ozone (ppb)"
  attr(df1$Wind,  "label") <- "Wind Speed (mph)"

  df2 <- mtcars[1:3, c("mpg", "cyl")]
  attr(df2$mpg, "label") <- "Miles Per Gallon"
  attr(df2$cyl, "label") <- "Number of Cylinders"

  result <- dict_label(list(air = df1, cars = df2))

  expect_type(result, "list")
  expect_named(result, c("air", "cars"))
})

test_that("dict_label() extracts labels correctly for each data frame in a named list", {
  df1 <- airquality[1:3, c("Ozone", "Wind")]
  attr(df1$Ozone, "label") <- "Ozone (ppb)"
  attr(df1$Wind,  "label") <- "Wind Speed (mph)"

  df2 <- mtcars[1:3, c("mpg", "cyl")]
  attr(df2$mpg, "label") <- "Miles Per Gallon"
  attr(df2$cyl, "label") <- "Number of Cylinders"

  result <- dict_label(list(air = df1, cars = df2))

  expect_equal(result$air$varcode,  c("Ozone", "Wind"))
  expect_equal(result$air$label,    c("Ozone (ppb)", "Wind Speed (mph)"))

  expect_equal(result$cars$varcode, c("mpg", "cyl"))
  expect_equal(result$cars$label,   c("Miles Per Gallon", "Number of Cylinders"))
})

test_that("dict_label() each element of the list result is a data frame", {
  df1 <- airquality[1:2, c("Ozone", "Wind")]
  df2 <- mtcars[1:2,    c("mpg", "cyl")]

  result <- dict_label(list(air = df1, cars = df2))

  expect_s3_class(result$air,  "data.frame")
  expect_s3_class(result$cars, "data.frame")
  expect_named(result$air,  c("varcode", "label"))
  expect_named(result$cars, c("varcode", "label"))
})

test_that("dict_label() handles mixed labelled/unlabelled columns in a list", {
  df1 <- airquality[1:3, c("Ozone", "Wind")]
  attr(df1$Ozone, "label") <- "Ozone (ppb)"
  # Wind left unlabelled

  df2 <- mtcars[1:3, c("mpg", "cyl")]
  # Both unlabelled

  result <- dict_label(list(air = df1, cars = df2))

  expect_equal(result$air$label,  c("Ozone (ppb)", ""))
  expect_equal(result$cars$label, c("", ""))
})

# ---------------------------------------------------------------------------
# sanitize argument
# ---------------------------------------------------------------------------

test_that("dict_label() sanitize = TRUE removes special characters from labels", {
  df <- airquality[1:2, c("Ozone", "Solar.R")]
  attr(df$Ozone,   "label") <- "Ozone <ppb> & 'level'"
  attr(df$Solar.R, "label") <- 'Solar "Radiation" >100%'

  result <- dict_label(df, sanitize = TRUE)

  expect_equal(result$label[1], "Ozone ppb  level")
  expect_equal(result$label[2], "Solar Radiation 100")
})

test_that("dict_label() sanitize = FALSE (default) preserves special characters", {
  df <- airquality[1:2, c("Ozone", "Solar.R")]
  attr(df$Ozone,   "label") <- "Ozone <ppb>"
  attr(df$Solar.R, "label") <- "Radiation & UV"

  result <- dict_label(df)

  expect_equal(result$label[1], "Ozone <ppb>")
  expect_equal(result$label[2], "Radiation & UV")
})

test_that("dict_label() sanitize applies to all data frames in a named list", {
  df1 <- airquality[1:2, c("Ozone", "Wind")]
  attr(df1$Ozone, "label") <- "Ozone <ppb>"
  attr(df1$Wind,  "label") <- "Wind & Speed"

  result <- dict_label(list(air = df1), sanitize = TRUE)

  expect_equal(result$air$label, c("Ozone ppb", "Wind  Speed"))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("dict_label() errors for a non-data-frame, non-list input", {
  expect_error(dict_label(1:10))
  expect_error(dict_label("a string"))
  expect_error(dict_label(NULL))
})

test_that("dict_label() errors for an unnamed list of data frames", {
  df <- airquality[1:3, c("Ozone", "Wind")]
  expect_error(dict_label(list(df, df)))
})

test_that("dict_label() errors for a list containing non-data-frame elements", {
  df <- airquality[1:3, c("Ozone", "Wind")]
  expect_error(dict_label(list(air = df, bad = 1:5)))
})

test_that("dict_label() errors when sanitize is not a single logical", {
  df <- airquality[1:3, c("Ozone", "Wind")]
  expect_error(dict_label(df, sanitize = "yes"))
  expect_error(dict_label(df, sanitize = c(TRUE, FALSE)))
})

test_that("dict_label() collapses multi-element label attributes with '; '", {
  df <- data.frame(a = 1)
  attr(df$a, "label") <- c("first", "second")

  result <- dict_label(df)

  expect_equal(result$label, "first; second")
})
