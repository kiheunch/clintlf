#' Extract PARAM metadata from ADaM datasets
#'
#' Summarizes the unique `PARAM`, `PARAMCD`, and `PARAMN` combinations in a
#' dataset, as found in parameter-based ADaM data (ADLB, ADVS, ADTTE, and
#' the like). `PARAM` is required; `PARAMCD` and `PARAMN` are included when
#' present. Given a named list, datasets without a `PARAM` column are
#' dropped with a message naming them; if none remain, an error is raised.
#'
#' @details
#' In ADaM, each analysis parameter is identified three ways:
#' \describe{
#'   \item{PARAM}{Full parameter description, e.g. `"Albumin (g/L)"`.}
#'   \item{PARAMCD}{Short parameter code, 8 characters or fewer, e.g. `"ALB"`.}
#'   \item{PARAMN}{Numeric parameter code, typically used for sorting.}
#' }
#'
#' @param x A data frame, or a named list of data frames.
#' @returns For a data frame: a data frame of unique `PARAMN`/`PARAMCD`/
#'   `PARAM` combinations, sorted by `PARAMN` when available. For a named
#'   list: a named list of such data frames.
#' @export
#' @examples
#' \dontrun{
#' # single ADaM data frame
#' adlb <- haven::read_xpt("adlb.xpt")
#' adlb_param <- dict_param(adlb)
#'
#' # named list of ADaM data frames
#' adam <- list(adlb = haven::read_xpt("adlb.xpt"),
#'              advs = haven::read_xpt("advs.xpt"))
#' adlb_param <- dict_param(adam)
#' }

dict_param <- function(x) {

  stopifnot(
  "x must be a data frame or a named list of data frames" =
    is.data.frame(x) ||
    (is.list(x) && !is.null(names(x)) && all(vapply(x, is.data.frame, FUN.VALUE = logical(1))))
  )

  # helper to summarize PARAM columns from a single data frame
  summarize_param <- function(df) {
    stopifnot("PARAM column is required" = "PARAM" %in% names(df))
    
    # determine which grouping columns are present
    grp_cols <- intersect(c("PARAMN", "PARAMCD", "PARAM"), names(df))

    # aggregate by unique combinations and sort
    agg <- unique(df[, grp_cols, drop = FALSE])
    if (nrow(agg) > nrow(unique(df[, "PARAM", drop = FALSE])))
      warning("Inconsistent (PARAMCD ~ PARAMN) combination detected for one or more PARAM values.")

    # sort by PARAMN if present, otherwise preserve original order
    if ("PARAMN" %in% grp_cols) {
      agg <- agg[order(agg$PARAMN), ]
    }

    rownames(agg) <- NULL
    agg
  }

  # single data frame input
  if (is.data.frame(x)) {
    return(summarize_param(x))
  }

  # named list input: drop datasets without PARAM, naming what was dropped
  has_param <- vapply(x, function(df) "PARAM" %in% names(df), FUN.VALUE = logical(1))

  if (any(!has_param))
    message("Dropped datasets without a PARAM column: ",
            paste(names(x)[!has_param], collapse = ", "))

  x <- x[has_param]

  if (length(x) == 0)
    stop("No dataset with PARAM column detected.")

  lapply(x, summarize_param)
}
