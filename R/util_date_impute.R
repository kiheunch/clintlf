#' Impute a single partial date string to yyyy-mm-dd format
#'
#' Internal. Strips any time component via `util_date_striptime()`, resolves
#' the separator, splits into components, and imputes missing month or day
#' values. Returns a complete yyyy-mm-dd date string.
#'
#' @param original A single partial or complete date or datetime string.
#' @param fill_month Zero-padded month to impute when missing, e.g. `"12"`.
#' @param fill_day Zero-padded day to impute when missing, e.g. `"15"`, or
#'   `NULL` to impute the last day of the resolved month via
#'   `util_date_lastday()`.
#' @param fmt Date component ordering: `"ymd"`, `"dmy"`, or `"mdy"`.
#' @param sep Separator for splitting; auto-detected via `util_date_sep()`
#'   when `NULL`.
#' @returns A single yyyy-mm-dd character string, or `NA_character_` when the
#'   input is `NA`, stripping the time leaves an empty string, all split
#'   components are empty or whitespace, or the input splits into more than
#'   three components.
#' @seealso [clintlf::util_date_striptime()], [clintlf::util_date_sep()], [impute_date()]
#' @noRd
#' @examples
#' \dontrun{
#' # util_date_impute() is intended to be called inside impute_date(), e.g.:
#' impute_date("2025-07")             # "2025-07-31"
#' impute_date("2025")                # "2025-12-31"
#' impute_date("2026-05-22T15:30:45") # "2026-05-22"
#' impute_date(NA)                    # NA_character_
#' }
util_date_impute <- function(original, fill_month, fill_day, fmt, sep) {
  if (is.na(original)) return(NA_character_)

  # strip time component
  date_str <- util_date_striptime(original)

  # guard: if stripping leaves an empty or whitespace-only string, return NA
  if (is.na(date_str) || nchar(trimws(date_str)) == 0)
    return(NA_character_)

  # resolve separator: user override or auto-detect
  effective_sep <- if (!is.null(sep)) sep else util_date_sep(date_str)

  # split into components
  p  <- strsplit(date_str, effective_sep, fixed = TRUE)[[1]]
  nd <- length(p) - 1  # number of separators = number of splits minus 1

  # guard: if all components are empty or whitespace-only after splitting,
  # the input could not be meaningfully parsed, return NA
  if (all(nchar(trimws(p)) == 0))
    return(NA_character_)

  # guard: more than 3 components (e.g. "2025-07-19-05") cannot be a date;
  # return NA rather than silently dropping the extra components
  if (length(p) > 3)
    return(NA_character_)

  pad_zero <- function(component) {
    ifelse(nchar(component) == 1, paste0("0", component), component)
  }

  # resolve the day fill: an explicit fill_day, or (when NULL) the last day of
  # the already-resolved year-month
  resolve_day <- function(yyyy, mm) {
    if (!is.null(fill_day)) fill_day else util_date_lastday(yyyy, mm)
  }

  if (fmt == "ymd") {
    yyyy <- p[1]
    mm   <- if (nd >= 1) pad_zero(p[2]) else fill_month
    dd   <- if (nd >= 2) pad_zero(p[3]) else resolve_day(yyyy, mm)

  } else if (fmt == "dmy") {
    if (nd == 0) {
      yyyy <- p[1]
      mm   <- fill_month
      dd   <- resolve_day(yyyy, mm)
    } else if (nd == 1) {
      yyyy <- p[2]
      mm   <- pad_zero(p[1])
      dd   <- resolve_day(yyyy, mm)
    } else {
      yyyy <- p[3]
      mm   <- pad_zero(p[2])
      dd   <- pad_zero(p[1])
    }

  } else if (fmt == "mdy") {
    if (nd == 0) {
      yyyy <- p[1]
      mm   <- fill_month
      dd   <- resolve_day(yyyy, mm)
    } else if (nd == 1) {
      yyyy <- p[2]
      mm   <- pad_zero(p[1])
      dd   <- resolve_day(yyyy, mm)
    } else {
      yyyy <- p[3]
      mm   <- pad_zero(p[1])
      dd   <- pad_zero(p[2])
    }
  }

  # a NULL fill_day against an unparseable month yields NA: signal parse failure
  if (is.na(dd)) return(NA_character_)

  paste0(yyyy, "-", mm, "-", dd)
}
