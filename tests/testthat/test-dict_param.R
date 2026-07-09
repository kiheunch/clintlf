# helper: build a labelled data frame without haven
make_labelled_df <- function() {
  df <- data.frame(
    USUBJID = character(),
    AGE     = integer(),
    SEX     = character(),
    stringsAsFactors = FALSE
  )
  df <- rbind(df, data.frame(
    USUBJID = c("001", "002"),
    AGE     = c(34L, 45L),
    SEX     = c("M", "F"),
    stringsAsFactors = FALSE
  ))
  attr(df$USUBJID, "label") <- "Unique Subject Identifier"
  attr(df$AGE,     "label") <- "Age in Years"
  # SEX intentionally left unlabelled
  df
}

make_bds_df <- function(paramn = TRUE) {
  df <- data.frame(
    USUBJID = c("001", "001", "002", "002"),
    PARAM   = c("Albumin", "Albumin", "Bilirubin", "Bilirubin"),
    PARAMCD = c("ALB", "ALB", "BILI", "BILI"),
    AVAL    = c(4.1, 4.2, 0.8, 0.9),
    stringsAsFactors = FALSE
  )
  if (paramn) df$PARAMN <- c(1L, 1L, 2L, 2L)
  df
}

# ── dict_param ────────────────────────────────────────────────────────────────

test_that("dict_param: single df returns data frame with expected columns", {
  out <- dict_param(make_bds_df())

  expect_s3_class(out, "data.frame")
  expect_true(all(c("PARAM", "PARAMCD", "PARAMN") %in% names(out)))
})

test_that("dict_param: returns unique PARAM combinations only", {
  out <- dict_param(make_bds_df())

  expect_equal(nrow(out), 2L)
  expect_equal(out$PARAM, c("Albumin", "Bilirubin"))
})

test_that("dict_param: sorted by PARAMN when present", {
  df        <- make_bds_df()
  df$PARAMN <- c(2L, 2L, 1L, 1L)  # deliberately reversed
  out       <- dict_param(df)

  expect_equal(out$PARAMN, c(1L, 2L))
})

test_that("dict_param: PARAMN absent — columns excluded, order preserved", {
  out <- dict_param(make_bds_df(paramn = FALSE))

  expect_false("PARAMN" %in% names(out))
  expect_equal(out$PARAM, c("Albumin", "Bilirubin"))
})

test_that("dict_param: PARAMCD absent — column excluded", {
  df      <- make_bds_df()
  df$PARAMCD <- NULL
  out     <- dict_param(df)

  expect_false("PARAMCD" %in% names(out))
  expect_true("PARAM" %in% names(out))
})

test_that("dict_param: warns on inconsistent PARAMCD for same PARAM", {
  df <- data.frame(
    PARAM   = c("Albumin", "Albumin"),
    PARAMCD = c("ALB", "ALB2"),   # inconsistent
    PARAMN  = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  expect_warning(dict_param(df), "Inconsistent")
})

test_that("dict_param: named list returns named list of data frames", {
  bds <- make_bds_df()
  out <- dict_param(list(adlb = bds, advs = bds))

  expect_type(out, "list")
  expect_named(out, c("adlb", "advs"))
  expect_s3_class(out$adlb, "data.frame")
})

test_that("dict_param: list drops datasets without PARAM with a message naming them", {
  bds      <- make_bds_df()
  no_param <- data.frame(USUBJID = "001", AVAL = 1.0)

  expect_message(
    out <- dict_param(list(adlb = bds, adsl = no_param)),
    "Dropped datasets without a PARAM column: adsl"
  )

  expect_named(out, "adlb")
  expect_null(out$adsl)
})

test_that("dict_param: errors when no dataset in list has PARAM", {
  no_param <- data.frame(USUBJID = "001", AVAL = 1.0)
  expect_error(
    suppressMessages(dict_param(list(adsl = no_param, adex = no_param))),
    "No dataset with PARAM column detected"
  )
})

test_that("dict_param: single df errors when PARAM absent", {
  expect_error(dict_param(data.frame(USUBJID = "001")), "PARAM column is required")
})

test_that("dict_param: rejects unnamed list", {
  expect_error(dict_param(list(make_bds_df(), make_bds_df())))
})

test_that("dict_param: rejects non-data-frame input", {
  expect_error(dict_param("not a data frame"))
  expect_error(dict_param(1:5))
})
