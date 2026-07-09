#' Strip the time component from a datetime string
#'
#' Internal. Removes the time portion of a single datetime string, keeping
#' only the date. Handles three forms:
#'
#' - ISO 8601 (`yyyy-MM-ddTHH:mm:ss`): split on `"T"`.
#' - US (`MM/dd/yyyy hh:mm tt`): split on whitespace; time and AM/PM tokens
#'   dropped.
#' - European/UK (`dd/MM/yyyy HH:mm:ss`): split on whitespace.
#'
#' Input without a time component passes through unchanged.
#'
#' @param element A single date or datetime string.
#' @returns The date portion as a character string, or `NA_character_` for
#'   `NA` input.
#' @noRd
#' @examples
#' \dontrun{
#' util_date_striptime("2026-05-22T15:30:45")  # "2026-05-22"
#' util_date_striptime("05/22/2026 03:30 PM")  # "05/22/2026"
#' util_date_striptime("22/05/2026 15:30:45")  # "22/05/2026"
#' util_date_striptime("2026-05-22")           # "2026-05-22" (unchanged)
#' util_date_striptime(NA)                     # NA_character_
#' }
util_date_striptime <- function(element) {
  if (is.na(element)) return(NA_character_)

  # ISO 8601: presence of "T" separating date and time
  if (grepl("T", element, fixed = TRUE)) {
    return(trimws(strsplit(element, "T", fixed = TRUE)[[1]][1]))
  }

  # US / European: presence of a space followed by time-like pattern
  # matches " HH:mm", " HH:mm:ss", " hh:mm AM/PM" etc.
  if (grepl("\\s+\\d{1,2}:\\d{2}", element)) {
    return(trimws(strsplit(element, "\\s+")[[1]][1]))
  }

  # no time component detected, return as-is
  element
}