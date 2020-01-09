parse_single_answer <- function(answer){
  # remove length-zero items as an initial fix to #38;
  if(!is.null(answer$tag_data)){
    answer$tag_data <- NULL
  }

  dplyr::bind_rows(answer)
}

parse_answers <- function(question){
  out_a <- purrr::map_df(question$answers, parse_single_answer) %>%
    dplyr::mutate(question_id = question$id)

  out_a
}

parse_page <- function(page){
  purrr::map_df(page$questions, parse_answers)
}

parse_response <- function(response){
  out <- purrr::map_df(response$pages, parse_page) %>%
    dplyr::mutate(response_id = response$id,
                  collector_id = response$collector_id,
                  survey_id = response$survey_id,
                  date_created = as.POSIXct(response$date_created, format = "%Y-%m-%dT%H:%M:%OS"),
                  date_modified = as.POSIXct(response$date_modified, format = "%Y-%m-%dT%H:%M:%OS"),
                  recipient_id = dplyr::if_else(response$recipient_id == "", NA_character_, response$recipient_id))

  if(length(response$ip_address) > 0 & response$ip_address != "") {
    out$ip_address <- response$ip_address
  }
  if(length(response$custom_variables) > 0) {
    out <- merge(out, dplyr::bind_rows(response$custom_variables))
  }
  if(length(response$metadata) > 0 ) {
    metdata_vars <- unlist(response$metadata)
    metdata_vars <- metdata_vars[grepl(".value$", names(metdata_vars))]
    metdata_vars_df <- bind_rows(metdata_vars)
    names(metdata_vars_df) <- names(metdata_vars_df) %>%
      gsub("^[A-z]+\\.", "", .) %>%
      gsub(".value$", "", .)
    out <- merge(out, metdata_vars_df)
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
