#' Auto-detect the separator of a date string
#'
#' Internal. Scans a single date string for `-`, `/`, or `.` and returns the
#' first match. With no separator present (a bare year, say), `"-"` is a safe
#' fallback: splitting on it yields one element, which the `nd == 0` branch
#' of `util_date_impute()` handles.
#'
#' @param element A single date string with any time component already
#'   stripped.
#' @returns One of `"-"`, `"/"`, or `"."`; `"-"` for `NA` input or when no
#'   separator is found.
#' @noRd
#' @examples
#' \dontrun{
#' util_date_sep("2025-07-19")  # "-"
#' util_date_sep("2025/07/19")  # "/"
#' util_date_sep("2025.07.19")  # "."
#' util_date_sep("2025")        # "-" (fallback)
#' util_date_sep(NA)            # "-" (fallback)
#' }
util_date_sep <- function(element) {
  if (is.na(element)) return("-")
  candidates <- c("-", "/", ".")
  for (candidate in candidates) {
    if (grepl(candidate, element, fixed = TRUE)) return(candidate)
  }
  "-"
}
