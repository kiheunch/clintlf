#' Extract variable labels from CDISC datasets
#'
#' Pulls the `"label"` attribute from each column, as carried by CDISC
#' datasets (ADSL, ADAE, and the like) read via e.g. `haven::read_xpt()`.
#' Columns without a label return an empty string. Handy for data
#' dictionaries and cross-reference tables.
#'
#' @param x A data frame, or a named list of data frames.
#' @param sanitize Strip characters that commonly break downstream rendering
#'   (`<`, `>`, `&`, `%`, quotes) from labels. Default `FALSE`.
#' @returns For a data frame: a data frame with columns `varcode` (column
#'   name) and `label` (`""` when none). For a named list: a named list of
#'   such data frames.
#' @export
#' @examples
#' \dontrun{
#' # single ADaM data frame
#' adsl <- haven::read_xpt("adsl.xpt")
#' dict_label(adsl)
#'
#' # named list of ADaM data frames
#' dict_label(list(adsl = haven::read_xpt("adsl.xpt"),
#'                 adae = haven::read_xpt("adae.xpt")))
#'
#' # remove special characters in labels in case of error
#' dict_label(adsl, sanitize = TRUE)
#' }

dict_label <- function(x, sanitize = FALSE) {

  stopifnot(
    "x must be a data frame or a named list of data frames" =
      is.data.frame(x) ||
      (is.list(x) && !is.null(names(x)) && all(vapply(x, is.data.frame, FUN.VALUE = logical(1))))
  )
  stopifnot(
    "sanitize must be logical" =
      is.logical(sanitize) && length(sanitize) == 1 && !is.na(sanitize)
  )

  # wrap single data frame into a list for uniform processing
  single_df_input <- is.data.frame(x)
  if (single_df_input) x <- list(x)

  result <- lapply(x, function(df) {
    labels <- vapply(df, function(col) {
      lbl <- attr(col, "label")
      lbl <- if (is.null(lbl)) "" else paste(as.character(lbl), collapse = "; ")
      if (sanitize) gsub("[<>&%\"']", "", lbl) else lbl
    }, FUN.VALUE = character(1))

    data.frame(
      varcode = names(df),
      label   = unname(labels)
    )
  })

  # unwrap back to single data frame if input was a single data frame
  if (single_df_input)
    return(result[[1]])
  else
    return(result)
}
