#' Pool estimates across imputations using Rubin's rules
#'
#' Pools estimates and standard errors across imputed datasets by Rubin's
#' rules, returning the pooled estimate, its confidence interval, and
#' two-sided p-value. Supply the per-imputation results as numeric vectors,
#' or as columns of a stacked data frame with one row per imputation.
#'
#' @param est Point estimates, one per imputation. With `data`, the name of
#'   the estimate column instead.
#' @param se Standard errors, one per imputation; positive and non-missing.
#'   With `data`, the name of the standard error column.
#' @param data Optional data frame with one row per imputation. When given,
#'   `est` and `se` are column names.
#' @param df Degrees-of-freedom method: `"1987"` (Rubin 1987; the SAS PROC
#'   MIANALYZE default) or `"1999"` (Barnard and Rubin 1999; matches
#'   `mice::pool()` and PROC MIANALYZE with EDF). Default `"1987"`.
#' @param conf Confidence level, in (0, 1). Default `0.95`.
#' @param n Sample size of each imputed dataset. Required for `df = "1999"`.
#' @param k Number of parameters in the fitted model. Required for
#'   `df = "1999"`.
#' @returns A 1-row data frame: `pool.est`, `pool.se` (square root of total
#'   variance), `lower`/`upper` confidence bounds, and two-sided `p.val`.
#' @references
#'   Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
#'
#'   Barnard, J. and Rubin, D.B. (1999). Small-sample degrees of freedom with
#'   multiple imputation. *Biometrika*, 86(4), 948-955.
#' @export
#' @importFrom stats qt pt
#' @examples
#' # vector interface
#' calc_poolmi(est = c(-2.57, -2.11, -1.85, -2.07, -2.13),
#'             se  = c( 1.05,  0.91,  0.92,  0.85,  0.90))
#'
#' # data frame interface (one row per imputation)
#' imp_result <- data.frame(
#'   IMPNO = 1:5,
#'   est   = c(-2.57, -2.11, -1.85, -2.07, -2.13),
#'   SE    = c( 1.05,  0.91,  0.92,  0.85,  0.90)
#' )
#' calc_poolmi(est = "est", se = "SE", data = imp_result)
#'
#' # see the package vignette for the full workflow using ADMI dataset,
#' # a cross-check against mice::pool(), and the equivalent SAS PROC
#' # MIANALYZE calls

calc_poolmi <- function(est,
                        se,
                        data    = NULL,
                        df      = c("1987", "1999"),
                        conf    = 0.95,
                        n       = NULL,
                        k       = NULL) {

  # --- resolve est/se: column names when data is given, vectors otherwise ---
  if (!is.null(data)) {
    stopifnot("data must be a data frame" = is.data.frame(data))
    stopifnot("est must be a single column name (character) when data is supplied" =
                is.character(est) && length(est) == 1)
    stopifnot("se must be a single column name (character) when data is supplied" =
                is.character(se) && length(se) == 1)

    if (!all(c(est, se) %in% colnames(data)))
      stop(paste0("Columns '", est, "' and '", se, "' not found in data. ",
                  "Please check est and se arguments."))

    est <- data[[est]]
    se  <- data[[se]]
  } else {
    stopifnot("est must be a numeric vector when data is not supplied" = is.numeric(est))
    stopifnot("se must be a numeric vector when data is not supplied" = is.numeric(se))
  }

  # --- input validation ---
  stopifnot("est must be numeric" = is.numeric(est))
  stopifnot("est contains NA values" = !anyNA(est))

  stopifnot("se must be numeric" = is.numeric(se))
  stopifnot("se contains NA values" = !anyNA(se))
  stopifnot("se contains non-positive values" = !any(se <= 0))

  stopifnot("est and se must be the same length" = length(est) == length(se))

  m <- length(est)
  stopifnot("at least 2 imputations (elements/rows) are required" = m >= 2)

  df <- match.arg(df)

  stopifnot("conf must be a single value between 0 and 1" =
              is.numeric(conf) && length(conf) == 1 && !is.na(conf) && conf > 0 && conf < 1)

  if (df == "1999") {
    if (is.null(n) || is.null(k))
      stop("n and k must be provided when df = '1999'.")
    stopifnot("n must be a positive integer scalar" =
                is.numeric(n) && length(n) == 1 && !is.na(n) && n > 0 && n %% 1 == 0)
    stopifnot("k must be a positive integer scalar" =
                is.numeric(k) && length(k) == 1 && !is.na(k) && k > 0 && k %% 1 == 0)
    if (n - k < 1)
      stop("k must be less than n (negative degree of freedom is not supported).")
  }

  # --- pool estimates ---
  pool.est         <- mean(est)
  pool.var.within  <- mean(se^2)
  pool.var.between <- sum((est - pool.est)^2) / (m - 1)
  pool.var.finite  <- pool.var.between / m
  pool.var.total   <- pool.var.within + pool.var.between + pool.var.finite
  pool.se          <- sqrt(pool.var.total)
  pool.wald        <- pool.est / pool.se
  lambda           <- (pool.var.between + pool.var.finite) / pool.var.total
  lambda           <- max(lambda, 1e-04)  # bound from mice 2.x mice.df(); keeps df finite when all est are equal

  # --- degrees of freedom ---
  df.old <- (m - 1) / lambda^2

  dfree <- if (df == "1999") {
    df.com <- n - k
    df.obs <- ((df.com + 1) / (df.com + 3)) * df.com * (1 - lambda)
    (df.old * df.obs) / (df.old + df.obs)
  } else {
    df.old
  }

  # --- confidence interval and p-value ---
  crit  <- qt((1 + conf) / 2, dfree)
  p.val <- 2 * pt(abs(pool.wald), dfree, lower.tail = FALSE)
  lower <- pool.est - crit * pool.se
  upper <- pool.est + crit * pool.se

  # --- output ---
  return(data.frame(pool.est = pool.est,
                    pool.se  = pool.se,
                    lower    = lower,
                    upper    = upper,
                    p.val    = p.val))
}
