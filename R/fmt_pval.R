#' Format a p-value with a reporting threshold
#'
#' Displays values below `alpha` as `"<alpha"` and rounds the rest half away
#' from zero.
#'
#' A value just above the threshold can round to the threshold itself: 0.051
#' with `n = 2` prints `"0.05"` next to `"<0.05"`. Use a larger `n` near the
#' threshold if that ambiguity matters.
#'
#' @param x Numeric vector of p-values in \[0, 1\]. `NA` prints as `"NA"`.
#' @param n Decimal places: a non-negative integer, scalar or one per element
#'   of `x`. Default `2`.
#' @param alpha Reporting threshold, in (0, 1); scalar or same length as
#'   `x`. Pass as character to keep trailing zeros in the label (`"0.10"`
#'   prints `"<0.10"`). Default `"0.05"`.
#' @param space Insert a space after `"<"`. Default `FALSE`.
#' @return Character vector of formatted p-values.
#' @seealso [fmt_round()]
#' @export
#' @examples
#' fmt_pval(0.055)
#' fmt_pval(c(0.051, 0.001, NA), n = 3, alpha = "0.05")
#' fmt_pval(c(0.051, 0.001), n = 3, alpha = c("0.05", "0.10"))
#' fmt_pval(0.03, alpha = 0.10)  # numeric alpha prints "<0.1"
fmt_pval <- function(x, n = 2L, alpha = "0.05", space = FALSE) {

  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("x must be between 0 and 1 (ignoring NA)" = all(x >= 0 & x <= 1, na.rm = TRUE))
  stopifnot("n must be a non-negative integer" = !anyNA(n) && all(n >= 0) && all(n %% 1 == 0))
  stopifnot("n must be scalar or same length as x" = length(n) == 1 || length(n) == length(x))
  stopifnot("alpha must be scalar or same length as x" = length(alpha) == 1 || length(alpha) == length(x))
  stopifnot("alpha must be numeric when coerced" = !anyNA(suppressWarnings(as.numeric(alpha))))
  stopifnot("space must be logical" = !is.na(space) && is.logical(space) && length(space) == 1)

  alpha_label <- as.character(alpha)
  alpha_num   <- as.numeric(alpha)

  stopifnot("alpha must be between 0 and 1" = all(alpha_num > 0 & alpha_num < 1))

  pad <- if (space) " " else ""

  result <- ifelse(
    x < alpha_num,
    paste0("<", pad, alpha_label),
    fmt_round(x, n)
  )

  result[is.na(x)] <- "NA"
  result
}
