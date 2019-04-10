
get_ind_question_info <- function(question){

    tibble::tibble(heading = question$headings[[1]]$heading,
                   question_id = question$id,
                   question_type = question$family,
                   question_subtype = question$subtype)
}

parse_page_of_questions <- function(page){
  purrr::map_df(page$questions, get_ind_question_info)

}



# Get answer choices
parse_answer_choices <- function(question){

  get_choice_elements <- function(x){
    x %>%
      dplyr::bind_rows() %>%
      dplyr::select(id, visible, text, position)
  }

  if(!is.null(question$answers$other)){ # some Qs don't have an "other" option
    other <-  get_choice_elements(question$answers$other)
  } else {
    other <- NULL
  }

  if(!is.null(question$answers) & is.null(question$answers$cols)){ # some basic Qs like comment box don't even have answer choices
    choices <- question$answers$choices %>%
      dplyr::bind_rows()
  } else if(!is.null(question$answers$cols)){ # menu matrix
    choices <- purrr::map_df(question$answers$cols, function(x) { get_choice_elements(x$choices)})
      }
    else {
      choices <- NULL
  }

  dplyr::bind_rows(choices, other) %>%
    dplyr::mutate(question_id = question$id)

}

parse_page_for_choices <- function(page){
  purrr::map_df(page$questions, parse_answer_choices)
}


## Now parse rows, aka subquestions

parse_rows <- function(question){
  if(question$subtype == "menu"){

    remove_choices <- function(col_list){
      col_list$choices <- NULL
      col_list
    }
    rows <- question$answers$cols %>%
      purrr::map(remove_choices) %>%
      purrr::map_df(dplyr::bind_rows)

  } else if(!is.null(question$answers$rows)){
    rows <- dplyr::bind_rows(question$answers$rows)
  } else {
    rows <- NULL
  }
  tibble::as_tibble(rows) %>%
    dplyr::mutate(question_id = question$id)
}

parse_page_for_rows <- function(page){
  purrr::map_df(page$questions, parse_rows)
}
