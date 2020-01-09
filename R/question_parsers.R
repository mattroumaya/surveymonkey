parse_page_of_questions <- function(page){
  purrr::map_df(page$questions, parse_question_info)
}

parse_all_questions <- function(surv_obj){
  out_q <- purrr::map_df(surv_obj$pages, parse_page_of_questions) %>%
    dplyr::filter(!question_type %in% "presentation")

  # Add columns required later if they weren't present, e.g., no "Other" options offered
  # code adapted from https://stackoverflow.com/a/45858044
  cols_to_require <- c("col_text", "other_text", "choice_text", "row_text")
  add <- cols_to_require[!cols_to_require %in% names(out_q)]
  if(length(add) != 0) out_q[add] <- NA_character_

  # remove "weight" and "description" columns if present, they appear to come from some ranking
  #   matrix questions and don't have a place in a CSV, given that choice text will appear.
  out_q$description <- NULL
  out_q$weight <- NULL
  out_q
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
  if(!is.null(other)) {
    out <- dplyr::bind_rows(out, other)%>%
      tidyr::fill(heading:question_subtype)
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
  } else {
    other <- NULL
  }
  other
}
