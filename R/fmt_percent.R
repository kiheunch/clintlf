#' Format a count with its percentage
#'
#' Combines a value and its percentage into `"x (p%)"`, rounding half away
#' from zero. By default, a positive count whose percentage rounds to zero is
#' shown as a `"<"` threshold (e.g. `"<0.1%"`), so a table never reports 0%
#' for a non-zero count. A zero denominator leaves the percentage undefined;
#' those elements display `zero_denom` instead (e.g. `"0 (-)"`).
#'
#' @param x Numeric vector of counts or values.
#' @param denom Denominator for the percentage. Scalar or same length as `x`.
#'   Defaults to `sum(x, na.rm = TRUE)`.
#' @param n Decimal places for the value. Default `0`.
#' @param perc_n Decimal places for the percentage. Defaults to `n`.
#' @param symbol Appended after the percentage. Default `"%"`; use `""` to
#'   omit.
#' @param show_lt Show a rounded-to-zero percentage of a positive `x` as its
#'   `"<"` threshold. Default `TRUE`; set to `FALSE` for non-count input.
#' @param zero_denom Shown in place of the percentage when the denominator is
#'   zero. Default `"-"`.
#' @returns Character vector in the form `"x (p%)"`. `NA` formats as
#'   `"NA (NA%)"`; a zero denominator as `"x (-)"` (see `zero_denom`).
#' @seealso [fmt_round()]
#' @export
#' @examples
#' fmt_percent(15, 100)
#' fmt_percent(15, 100, n = 1)
#' fmt_percent(c(30, 50, 20), perc_n = 1, symbol = "")
#' fmt_percent(0, 0)
#' fmt_percent(c(0, 0), zero_denom = "NA")

fmt_percent <- function(x, denom = NULL, n = 0L, perc_n = NULL, symbol = "%",
                        show_lt = TRUE, zero_denom = "-") {

  stopifnot("x must be numeric" = is.numeric(x))

  if (is.null(denom))  denom  <- sum(x, na.rm = TRUE)
  if (is.null(perc_n)) perc_n <- n

  stopifnot("denom must be numeric and non-missing"      = is.numeric(denom) & !anyNA(denom))
  stopifnot("n must be a non-negative integer"           = all(n >= 0, n %% 1 == 0))
  stopifnot("perc_n must be a non-negative integer"      = all(perc_n >= 0, perc_n %% 1 == 0))
  stopifnot("symbol must be a character string"          = is.character(symbol) & length(symbol) == 1)
  stopifnot("denom must be a scalar or same length as x" = length(denom) == 1 | length(denom) == length(x))
  stopifnot("show_lt must be a logical scalar"           = is.logical(show_lt) & length(show_lt) == 1 & !is.na(show_lt))
  stopifnot("zero_denom must be a character string"      = is.character(zero_denom) & length(zero_denom) == 1 & !is.na(zero_denom))

  perc       <- x / denom * 100
  round_x    <- fmt_round(x, n = n)
  round_perc <- fmt_round(perc, n = perc_n)

  if (show_lt) {
    zero_label    <- formatC(0, digits = perc_n, format = "f")
    threshold_str <- formatC(10^-perc_n, digits = perc_n, format = "f")
    lt            <- !is.na(x) & x > 0 & round_perc == zero_label
    round_perc[lt] <- paste0("<", threshold_str)
  }

  out <- paste0(round_x, " (", round_perc, symbol, ")")

  # zero denominator: percentage undefined, show zero_denom without symbol
  undefined <- rep_len(denom == 0, length(x))
  out[undefined] <- paste0(round_x[undefined], " (", zero_denom, ")")
  out
}
