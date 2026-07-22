#' Last calendar day of a given year-month
#'
#' Internal. Returns the last day of the month identified by `yyyy` and `mm`
#' as a zero-padded string, accounting for leap years in February. Used by
#' `util_date_impute()` to resolve the default `fill_day` when the caller
#' leaves it `NULL` ("last day of the respective month").
#'
#' @param yyyy Year component, e.g. `"2025"`. Only consulted for February.
#' @param mm Month component, `"01"`-`"12"`.
#' @returns Zero-padded day string (`"28"`, `"29"`, `"30"`, or `"31"`), or
#'   `NA_character_` when `mm` is missing or out of range.
#' @noRd
#' @examples
#' \dontrun{
#' util_date_lastday("2025", "12")  # "31"
#' util_date_lastday("2024", "02")  # "29" (leap year)
#' util_date_lastday("2025", "02")  # "28"
#' }
util_date_lastday <- function(yyyy, mm) {
  m <- suppressWarnings(as.integer(mm))
  y <- suppressWarnings(as.integer(yyyy))
  if (is.na(m) || m < 1 || m > 12) return(NA_character_)

  days    <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  d       <- days[m]
  is_leap <- !is.na(y) && ((y %% 4 == 0 && y %% 100 != 0) || y %% 400 == 0)
  if (m == 2 && is_leap) d <- 29

  sprintf("%02d", d)
}
