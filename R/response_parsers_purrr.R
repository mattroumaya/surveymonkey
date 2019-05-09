parse_single_answer <- function(answer){
  dplyr::bind_rows(answer)
}

parse_answers <- function(question){
  purrr::map_df(question$answers, parse_single_answer) %>%
    dplyr::mutate(question_id = question$id)
}

parse_page <- function(page){
  purrr::map_df(page$questions, parse_answers)
}

parse_response <- function(response){
  out <- purrr::map_df(response$pages, parse_page) %>%
    dplyr::mutate(response_id = response$id,
                  collector_id = response$collector_id,
                  survey_id = response$survey_id,
                  recipient_id = dplyr::if_else(response$recipient_id == "", NA_character_, response$recipient_id),
                  date_created = response$date_created,
                  date_modified = response$date_modified)
  if(!is.null(response$ip_address)){ out$ip_address <- response$ip_address }

  # append custom variables
  if(!is.null(response$custom_variables) & length(response$custom_variables) > 0){
    custom_vars <- bind_rows(response$custom_variables)
    out <- cbind(out, custom_vars)
  }
  out

}

parse_respondent_list <- function(respondents){
  out_resps <- purrr::map_df(respondents, parse_response)
  if(!"other_id" %in% names(out_resps)){
    out_resps$other_id <- NA_character_
  }
  if(!"text" %in% names(out_resps)){
    out_resps$text <- NA_character_
  }
  if(!"row_id" %in% names(out_resps)){
    out_resps$row_id <- NA_character_
  }
  if(!"col_id" %in% names(out_resps)){
    out_resps$col_id <- NA_character_
  }

  out_resps %>%
    dplyr::rename(response_text = text) %>%
    dplyr::select(survey_id, collector_id, recipient_id, response_id, dplyr::everything()) %>%
    dplyr::mutate(survey_id = as.numeric(survey_id))
}
