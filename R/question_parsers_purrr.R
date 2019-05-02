parse_page_of_questions <- function(page){
  purrr::map_df(page$questions, parse_question_info)
}

parse_all_questions <- function(surv_obj){
  out <- purrr::map_df(surv_obj$pages, parse_page_of_questions)
  # TODO - guarantee here that if key columns were missing from all question types, they are forced into existence
}


# New function per #21 to grab all Q+A info at once
parse_question_info <- function(ques){

  # get top-level info
  q_info <- tibble::tibble(heading = ques$headings[[1]]$heading,
                 question_id = ques$id,
                 question_type = ques$family,
                 question_subtype = ques$subtype)

  cols <- parse_cols(ques)
  rows <- parse_rows(ques)
  rows$position <- NULL # in the way right now so removing if exists
  other <- parse_other(ques)

  # choices live in cols if cols exists, so pull them out - not positive about this.
  if(!is.null(cols)){
    choices <- cols %>%
      dplyr::select(choice_id, choice_text, position, col_id)
    cols <- cols %>%
      dplyr::select(-choice_id, -choice_text, -position) %>%
      dplyr::distinct(.keep_all = TRUE)
  } else { # otherwise get choices from the other place they live
    choices <- parse_choices(ques)
  }

  # TODO - right now choices are not used.
  # Are they joined in to responses later?  And used for factor levels?  But not here?
  # If that's the case, break out into a separate function?

  # join them

  out <- q_info
  if(!is.null(rows)) { out <- merge(out, rows) }
  if(!is.null(cols)) { out <- merge(out, cols) }
  if(!is.null(other)) { out <- merge(out, other) }

  # TODO - create unique ID here?

  tibble::as_tibble(out)


}

## These functions below are called by parse_question_info

# Takes one col, returns data.frame
parse_cols <- function(ques){
  get_single_col_info <- function(col){
    dplyr::bind_rows(col$choices) %>%
      dplyr::rename(choice_id = id, choice_text = text) %>%
      dplyr::mutate(col_id = col$id,
                    col_text = col$text)  %>%
      dplyr::select(-visible, -is_na)
  }
  if(!is.null(ques$answers$cols)){
    cols <- purrr::map_df(ques$answers$cols, get_single_col_info)
  } else {
    cols <- NULL
  }
  cols
}

parse_rows <- function(question){
  if(!is.null(question$answers$rows)){
    rows <- dplyr::bind_rows(question$answers$rows) %>%
      dplyr::rename(row_id = id, row_text = text) %>%
      dplyr::select(-visible)
  } else {
    rows <- NULL
  }
  rows
}

parse_choices <- function(question){
  if(!is.null(question$answers$choices)){
    choices <- dplyr::bind_rows(question$answers$choices) %>%
      dplyr::rename(choice_id = id, choice_text = text) %>%
      dplyr::select(-visible)
    choices$is_na <- NULL # won't always exist, remove if it does
  } else {
    choices <- NULL
  }
  choices
}

parse_other <- function(question){
  if(!is.null(question$answers$other)){
    other <- dplyr::bind_rows(question$answers$other) %>%
      dplyr::rename(other_id = id, other_text = text) %>%
      dplyr::select(other_id, other_text) # don't think we'll need columns besides these

    # create a non-other row for the vanilla version of the question, too
    other2 <- bind_rows(other, other)
    other2$other_id[nrow(other2)] <- NA
    other2$other_text[nrow(other2)] <- NA
  } else {
    other2 <- NULL
  }
  other2
}
