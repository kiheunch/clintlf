#' Format a descriptive statistics cell
#'
#' The general cell builder: a lead value followed by a bracketed group,
#' `"x (a, b)"`, rounding half away from zero. One function covers the usual
#' summary cells: `estimate (LowerCI, UpperCI)`, `mean (SD)`,
#' `median (Q1, Q3)`, or `min, max` with no lead and no brackets.
#'
#' @param x Lead value shown before the brackets. Numeric or character
#'   coercible to numeric; `NULL` for a cell with no lead such as
#'   `"min, max"`.
#' @param ... One or more vectors for the bracketed group, in display order,
#'   each the same length as `x`. Numeric or character coercible to numeric.
#' @param n Decimal places for the lead. Default `1`.
#' @param inner_n Decimal places for the bracketed values. Default `1`.
#' @param bracket Opening bracket: `"("`, `"["`, or `""` for none. The
#'   closing bracket follows. Default `"("`.
#' @param sep Separator between bracketed values. Default `", "`.
#' @param na Shown in place of a missing value, e.g. `"-"` for the SD of a
#'   single observation. `NaN` displays the same way. Default `"NA"`.
#' @param eps Apply the floating-point correction of [fmt_round()].
#'   Default `TRUE`.
#' @returns Character vector, one cell per element, e.g. `"23.5 (4.57)"`.
#' @seealso [fmt_round()]
#' @export
#' @examples
#' fmt_stat(23.456, 4.5678, inner_n = 2)                    # mean (SD)
#' fmt_stat(5.678, 2.345, 8.901)                            # median (Q1, Q3)
#' fmt_stat(3.14159, 1.23456, 5.6789, n = 2, inner_n = 2)   # estimate (95% CI)
#' fmt_stat(NULL, 0.123, 9.876, bracket = "")               # min, max
#' fmt_stat(c(12.3, 15.8), c(4.56, NA), na = "-")           # SD missing for n = 1

fmt_stat <- function(x = NULL, ..., n = 1L, inner_n = 1L, bracket = "(",
                     sep = ", ", na = "NA", eps = TRUE) {

  inner <- list(...)

  stopifnot("supply at least one bracketed value in ..." = length(inner) > 0)
  stopifnot("vectors in ... must be unnamed; check for misspelled arguments" =
    is.null(names(inner)) || all(names(inner) == ""))

  coerce_to_numeric <- function(v, label) {
    if (is.numeric(v) || all(is.na(v))) return(as.numeric(v))
    if (is.character(v)) {
      out <- suppressWarnings(as.numeric(v))
      if (any(is.na(out) & !is.na(v)))
        stop(label, " contains values that cannot be coerced to numeric.")
      return(out)
    }
    stop(label, " must be numeric or character.")
  }

  if (!is.null(x)) x <- coerce_to_numeric(x, "'x'")
  inner <- Map(coerce_to_numeric, inner, paste0("vector ", seq_along(inner), " in ..."))

  len <- length(inner[[1]])
  stopifnot("all values must be the same length" =
    all(lengths(inner) == len) && (is.null(x) || length(x) == len))

  stopifnot("n must be a non-negative integer scalar" =
    !is.na(n) && length(n) == 1 && n >= 0 && n %% 1 == 0)
  stopifnot("inner_n must be a non-negative integer scalar" =
    !is.na(inner_n) && length(inner_n) == 1 && inner_n >= 0 && inner_n %% 1 == 0)
  stopifnot("bracket must be \"(\", \"[\", or \"\"" =
    is.character(bracket) && length(bracket) == 1 && bracket %in% c("(", "[", ""))
  stopifnot("sep must be a character string" =
    is.character(sep) && length(sep) == 1 && !is.na(sep))
  stopifnot("na must be a character string" =
    is.character(na) && length(na) == 1 && !is.na(na))

  if (len == 0) return(character(0))

  round_na <- function(v, digits) {
    out <- fmt_round(v, digits, eps = eps)
    out[is.na(v)] <- na
    out
  }

  close <- if (bracket == "") "" else c("(" = ")", "[" = "]")[[bracket]]
  inner_str <- do.call(paste, c(lapply(inner, round_na, digits = inner_n), sep = sep))
  group <- paste0(bracket, inner_str, close)

  if (is.null(x)) group else paste(round_na(x, n), group)
}
