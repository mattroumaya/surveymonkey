#' @title strip_html
#'
#' @description
#' Remove all values between "<>", which are typically HTML tags.
#'
#' @param dat a data.frame.
#' @param ignore a character vector containing values to ignore while stripping HTML tags. For instance, if you have <keep me> and <me too> in your column names, add ignore = c("keep me", "me too").
#'
#'
#' @return a data.frame object.
#'
#' @export
strip_html <- function(dat, ignore = NULL){

  regex.escape <- function(string) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", string)
  }



  if (is.null(ignore)){
    check_ignore <- ""
    names(dat) <- gsub("(<[^>]*>)","",names(dat))
  }

  if (!is.null(ignore)){
   check_ignore <- paste(ignore, collapse = "|")
   names(dat) <- gsub(paste0("<(?!(?:", paste(regex.escape(ignore), collapse="|"), ")>)[^>]*>"), "", names(dat), perl=TRUE)
  }

  if (!is.null(ignore) & any(grepl(check_ignore, names(dat))) == F){
    warning("None of your ignored values were found. All text between <> will be removed.")
  }



  return(as.data.frame(dat))

}






