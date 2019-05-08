parse_page_of_questions <- function(page){
  purrr::map_df(page$questions, parse_question_info)
}

parse_all_questions <- function(surv_obj){
  out <- purrr::map_df(surv_obj$pages, parse_page_of_questions) %>%
    dplyr::filter(!question_type %in% "presentation")

  # TODO - guarantee here that if key columns were missing from all question types, they are forced into existence

  # TODO - create unique ID here?
  ## No, do this after merging with response data on question, row, col ID
  ## At that point, if question type = Multiple Choice, include choice text + ID in the combined new columns

  ### Move this to after the merge with responses but here's a start...
  out$q_unique_id <- apply(
    out %>%
      select(question_id, row_id, col_id, other_id),
    1,
    function(x) paste(na.omit(x), collapse="_")
  )

  out$combined_q_heading <- apply(
    out %>%
      select(heading, row_text, col_text, other_text),
    1,
    function(x) paste(na.omit(x), collapse=" - ")
  )

## And here's some old code to look at and adapt re: multiple choice
  out %>%
    dplyr::mutate(combined_text = dplyr::case_when( # most of what will eventually be column headers - but problematic for duplicated Qs
      !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
      !is.na(open_response_text) & question_type != "open_ended" ~ paste(heading, text, sep = " - "), # for "Other (please specify)"
      question_type %in% "multiple_choice" ~ paste(heading, text, sep = " - "), # expand multiple choice Qs to be one-column-per
      TRUE ~ heading)) %>%
    dplyr::mutate(unique_q_id = dplyr::case_when( # spread and join on this as it works for duplicated Q text
      !is.na(subquestion_text) ~ paste(question_id, subquestion_id, sep = "_"),
      !is.na(open_response_text) & question_type != "open_ended" ~ paste(question_id, choice_id, sep = "_"), # for "Other (please specify)"
      question_type %in% "multiple_choice" ~ paste(question_id, choice_id, sep = "_"), # expand multiple choice Qs to be one-column-per
      TRUE ~ question_id
    )) %>%
    dplyr::mutate(text = dplyr::case_when(
      !is.na(open_response_text) ~ open_response_text, # replace with "Other" text when present
      TRUE ~ text)
    )

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
      dplyr::select(choice_id, choice_text, choice_position = position, col_id)
    cols <- cols %>%
      dplyr::select(-choice_id, -choice_text, -position) %>%
      dplyr::distinct(.keep_all = TRUE)
  } else { # otherwise get choices from the other place they live
    choices <- parse_choices(ques)
  }

  # join them
  # then will join with responses on unique ID (Q/row/col) & choice_id
  out <- q_info
  if(!is.null(rows)) { out <- merge(out, rows) }
  if(!is.null(cols)) { out <- merge(out, cols) }
  if(!is.null(choices)) { out <- merge(out, choices) }
  if(!is.null(other)) { out <- merge(out, other) }


  # parse_other adds a dummy 2nd row, so then this last merge causes unwanted duplicates
  # trim all but one, the one corresponding to "Other"
  if(!is.null(other) & (nrow(out) > 2)){
    out <- out %>%
      filter(is.na(other_id) | choice_id == max(choice_id))
  }
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
      dplyr::rename(choice_id = id, choice_text = text, choice_position = position) %>%
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
    other2 <- dplyr::bind_rows(other, other)
    other2$other_id[nrow(other2)] <- NA
    other2$other_text[nrow(other2)] <- NA
  } else {
    other2 <- NULL
  }
  other2
}
