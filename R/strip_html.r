#' @title strip_html
#'
#' @description
#' Remove all values between "<>", which are typically HTML tags.
#'
#' @param dat a data.frame.
#' @param ignore a character vector containing values to ignore while stripping HTML tags.
#' For instance, if you have <keep me> and <me too> in your column names,
#' add ignore = c("keep me", "me too").
#' @param trim_space logical, if TRUE trim extra white space
#'
#'
#' @return a data.frame object.
#'
#' @examples
#' \dontrun{
#' fetch_survey_obj(1234567890) %>%
#'   parse_survey() %>%
#'   strip_html()
#' }
#'
#' @export
strip_html <- function(dat,
                       ignore = NULL,
                       trim_space = TRUE) {
  regex_escape <- function(string) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
  }

  if (is.null(ignore)) {
    check_ignore <- ""
    names(dat) <- gsub("(<[^>]*>)", "", names(dat))
  } else {
    check_ignore <- paste(ignore, collapse = "|")
  }

  if (!is.null(ignore) & !any(grepl(check_ignore, names(dat)))) {
    warning("None of your ignored values were found. All text between <> will be removed.")
  }

  if (!is.null(ignore)) {
    names(dat) <- gsub(paste0(
      "<(?!(?:",
      paste(
        regex_escape(ignore),
        collapse = "|"
      ),
      ")>)[^>]*>"
    ), "", names(dat), perl = TRUE)
  }

  if (trim_space == TRUE) {
    names(dat) <- trimws(gsub("\\s+", " ", names(dat)))
  }

  return(dat)
}
