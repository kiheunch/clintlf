#' Round half away from zero, formatted for display
#'
#' Rounds half away from zero (commercial rounding, as in SAS `round()`;
#' base R rounds half to even) and returns character output with trailing
#' zeros kept, ready for table display. Negative values round symmetrically:
#' -2.5 becomes -3.
#'
#' @details
#' With `eps = TRUE`, a correction of `.Machine$double.eps * abs(x) * 10^n`
#' is added before truncation to offset binary representation error:
#' `abs(x)` tracks the floating-point spacing of the input, `10^n` the shift
#' to the rounding scale. This reproduces SAS `round()` on values stored just
#' under a half boundary, but can overcorrect when `abs(x)` or `n` is
#' extreme. A fixed correction such as `sqrt(.Machine$double.eps)` adapts to
#' neither.
#'
#' @param x Numeric vector to round.
#' @param n Decimal places: a non-negative integer, scalar or one per element
#'   of `x`. Default `2`.
#' @param eps Apply the scaled floating-point correction. Default `TRUE`.
#'
#' @return Character vector, same length as `x`, formatted to `n` decimal
#'   places. `NA` and `NaN` become `"NA"` and `"NaN"`.
#'
#' @references
#' Based on:
#' * \url{https://stackoverflow.com/questions/12688717/round-up-from-5}
#' * \url{https://andrewlandgraf.com/2012/06/15/rounding-in-r/}
#'
#' The author's scaled correction is proposed at
#' \url{https://stackoverflow.com/questions/12688717/round-up-from-5/79962354#79962354}.
#'
#' For comparison in SAS, see: \code{round()}, \code{rounde()}
#' @export
#' @examples
#' round(c(9.5, 10.5), 0)
#' fmt_round(c(9.5, 10.5), 0)
#'
#' fmt_round(0.0001, n = 2)
#' fmt_round(0.1, n = 2)
#'
#' fmt_round(c(2.5, 0.035, NA), c(0, 2, 2))

fmt_round <- function(x, n = 2L, eps = TRUE){
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("n must be a non-negative integer" = !anyNA(n) && all(n >= 0) && all(n %% 1 == 0))
  stopifnot("n must be scalar or same length as x" = length(n) == 1 || length(n) == length(x))
  stopifnot("eps must be a non-missing logical scalar" = !is.na(eps) && is.logical(eps) && length(eps) == 1)

  if (length(x) == 0) return(character(0))

  round2 <- function(x, n) {
    correction <- if (eps) .Machine$double.eps * abs(x) * 10^n else 0
    posneg <- sign(x)
    z <- abs(x) * 10^n
    z <- z + 0.5 + correction
    z <- floor(z)
    z <- z / 10^n
    z * posneg + 0  # + 0 turns IEEE -0 into 0, so -0.001 prints "0.00" not "-0.00"
  }

  result <- mapply(function(xi, ni) {
    round2(xi, ni) |> formatC(digits = ni, format = "f") |> trimws()
  }, x, n, USE.NAMES = FALSE)

  result[is.na(x)]  <- "NA"   # covers NA and NaN
  result[is.nan(x)] <- "NaN"  # overwrites NaN entries
  return(result)
}
