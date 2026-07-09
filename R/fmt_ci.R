#' Format an estimate with its confidence interval
#'
#' Combines a point estimate and its interval bounds into
#' `"est (lower, upper)"`, rounding half away from zero.
#'
#' @param est,lci,uci Point estimate and lower/upper confidence bounds.
#'   Numeric, or character coercible to numeric. All three must share the
#'   same length.
#' @param est_n Decimal places for the estimate. Default `2`.
#' @param ci_n Decimal places for the bounds. Defaults to `est_n`.
#' @param ... Passed to [fmt_round()], e.g. `eps`.
#' @returns Character vector in the form `"est (lower, upper)"`.
#' @seealso [fmt_round()]
#' @export
#' @examples
#' fmt_ci(3.14159, 1.23456, 5.67890)
#' fmt_ci(3.14159, 1.23456, 5.67890, est_n = 2, ci_n = 1)
#' fmt_ci(c(1.111, 2.222), c(0.500, 1.500), c(1.700, 2.900))

fmt_ci <- function(est, lci, uci, est_n = 2L, ci_n = est_n, ...) {

  coerce_to_numeric <- function(v, name) {
    if (is.numeric(v) || all(is.na(v))) return(as.numeric(v))
    if (is.character(v)) {
      out <- suppressWarnings(as.numeric(v))
      if (any(is.na(out) & !is.na(v)))
        stop(paste0("'", name, "' contains values that cannot be coerced to numeric."))
      return(out)
    }
    stop(paste0("'", name, "' must be numeric or character."))
  }

  est <- coerce_to_numeric(est, "est")
  lci <- coerce_to_numeric(lci, "lci")
  uci <- coerce_to_numeric(uci, "uci")

  stopifnot("est, lci, uci must be the same length" = length(est) == length(lci) && length(lci) == length(uci))
  stopifnot("est, lci, uci must not be empty" = length(est) > 0)
  stopifnot("est_n must be a non-negative integer scalar" =
    !is.na(est_n) && length(est_n) == 1 && est_n >= 0 && est_n %% 1 == 0)
  stopifnot("ci_n must be a non-negative integer scalar" =
    !is.na(ci_n) && length(ci_n) == 1 && ci_n >= 0 && ci_n %% 1 == 0)

  round_est <- fmt_round(est, est_n, ...)
  round_lci <- fmt_round(lci, ci_n, ...)
  round_uci <- fmt_round(uci, ci_n, ...)

  paste0(round_est, " (", round_lci, ", ", round_uci, ")")
}
