#' Impute partial dates to complete yyyy-mm-dd format
#'
#' Fills in missing month and day components of partial date strings.
#' Handles `"ymd"` (ISO 8601), `"mdy"` (US), and `"dmy"` (European)
#' orderings, auto-detects the separator (`-`, `/`, `.`), and strips time
#' components before imputing.
#'
#' Malformed or out-of-range output raises a warning, but calendar validity
#' is not checked: fill values can intentionally create dates such as
#' "2025-02-31". Validate with `as.Date()` when calendar-valid dates are
#' required.
#'
#' @param x Character vector of partial dates or datetimes, e.g.
#'   `c("2025-07-19", "2025-07", "2025", "2026-05-22T15:30:45")`.
#' @param fill_month Month to impute when missing. Default `"01"`.
#' @param fill_day Day to impute when missing. Default `"01"`.
#' @param fmt Ordering of the input date components: `"ymd"` (default),
#'   `"dmy"`, or `"mdy"`.
#' @param sep Separator between date components: `"-"`, `"/"`, or `"."`.
#'   Auto-detected per element when `NULL` (default).
#' @returns Character vector of complete yyyy-mm-dd dates.
#' @export
#' @examples
#' impute_date(c("2025-07-19", "2025-07", "2025", NA))
#' impute_date("2025", fill_month = "12", fill_day = "31")
#' impute_date("2025/07")                  # separator auto-detected
#' impute_date("07-19-2025", fmt = "mdy")
#' impute_date("2026-05-22T15:30:45")      # time stripped

impute_date <- function(x,
                     fill_month = "01",
                     fill_day   = "01",
                     fmt        = c("ymd", "dmy", "mdy"),
                     sep        = NULL) {

  # --- input validation ---
  if (!is.character(x) && !all(is.na(x)))
    stop("x must be a character vector.")
  x <- as.character(x)

  stopifnot("fill_month must be a valid month (01-12)" =
              is.character(fill_month) & length(fill_month) == 1 &
              suppressWarnings(as.integer(fill_month)) %in% 1:12)
  stopifnot("fill_day must be a valid day (01-31)" =
              is.character(fill_day) & length(fill_day) == 1 &
              suppressWarnings(as.integer(fill_day)) %in% 1:31)

  if (!is.null(sep)) {
    stopifnot("sep must be a single character string" =
                is.character(sep) & length(sep) == 1)
  }

  fmt <- match.arg(fmt)

  # --- pad fill values ---
  pad_zero <- function(component) {
    ifelse(nchar(component) == 1, paste0("0", component), component)
  }

  fill_month <- pad_zero(fill_month)
  fill_day   <- pad_zero(fill_day)

  # --- apply imputation over vector ---
  out <- vapply(
    x,
    util_date_impute,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    fill_month = fill_month,
    fill_day   = fill_day,
    fmt        = fmt,
    sep        = sep
  )

  # --- output checks ---
  # check 1: confirm NA count has not increased (i.e., no new NAs introduced)
  na_in  <- which(is.na(x))
  na_out <- which(is.na(out))
  new_na <- setdiff(na_out, na_in)

  if (length(new_na) > 0) {
    warning(
      "The following input elements were non-missing but produced NA in the ",
      "output, suggesting a parsing failure. Check these indices: ",
      paste(new_na, collapse = ", "), "."
    )
  }

  # check 2: confirm all non-missing output conforms to yyyy-mm-dd format.
  # Two sub-checks are applied:
  #   (a) character length must be exactly 10 (yyyy-mm-dd = 10 chars)
  #   (b) string must match the pattern dddd-dd-dd where d is a digit
  non_missing_idx <- which(!is.na(out))
  non_missing_out <- out[non_missing_idx]

  wrong_length  <- non_missing_idx[nchar(non_missing_out) != 10]
  wrong_pattern <- non_missing_idx[!grepl("^\\d{4}-\\d{2}-\\d{2}$", non_missing_out)]
  malformed     <- sort(union(wrong_length, wrong_pattern))

  if (length(malformed) > 0) {
    warning(
      "The following output elements do not conform to yyyy-mm-dd format. ",
      "Review the input values and fmt/sep arguments at these indices: ",
      paste(malformed, collapse = ", "), "."
    )
  }

  # check 3: for structurally valid output, confirm month is 01-12 and day is
  # 01-31. Calendar validity (e.g., Feb 30) is not checked; fill values may
  # intentionally create such dates, so validate with as.Date() separately.
  valid_idx  <- setdiff(non_missing_idx, malformed)
  mm         <- as.integer(substr(out[valid_idx], 6, 7))
  dd         <- as.integer(substr(out[valid_idx], 9, 10))
  out_of_rng <- valid_idx[mm < 1 | mm > 12 | dd < 1 | dd > 31]

  if (length(out_of_rng) > 0) {
    warning(
      "The following output elements contain an out-of-range month or day ",
      "component. Review the input values and fmt/sep arguments at these ",
      "indices: ", paste(out_of_rng, collapse = ", "), "."
    )
  }

  return(out)
}
