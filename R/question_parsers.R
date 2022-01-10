parse_page_of_questions <- function(page) {
  purrr::map_df(page$questions, parse_question_info)
}

#' @improtFrom rlang .data
parse_all_questions <- function(surv_obj) {
  out_q <- purrr::map_df(surv_obj$pages, parse_page_of_questions) %>%
    dplyr::filter(!.data$question_type %in% "presentation")

  # Add columns required later if they weren't present, e.g., no "Other" options offered
  # code adapted from https://stackoverflow.com/a/45858044
  cols_to_require <- c("col_text", "other_text", "choice_text", "row_text")
  add <- cols_to_require[!cols_to_require %in% names(out_q)]
  if (length(add) != 0) out_q[add] <- NA_character_

  # remove "weight" and "description" columns if present, they appear to come from some ranking
  #   matrix questions and don't have a place in a CSV, given that choice text will appear.
  out_q$description <- NULL
  out_q$weight <- NULL
  out_q
}


#' @importFrom rlang .data
# New function per #21 to grab all Q+A info at once
parse_question_info <- function(ques) {

  # Fixes issue 65 - this was caused by an image/presentation type not having a value for 'heading'
  if (is.null(ques$headings[[1]]$heading)) {
    ques$headings[[1]]$heading <- NA_character_
  }

  # get top-level info
  q_info <- tibble::tibble(
    heading = ques$headings[[1]]$heading,
    question_id = ques$id,
    question_type = ques$family,
    question_subtype = ques$subtype
  )

  cols <- parse_cols(ques)
  rows <- parse_rows(ques)
  rows$position <- NULL # in the way right now so removing if exists
  other <- parse_other(ques)

  # choices live in cols if cols exists, so pull them out - not positive about this.
  if (!is.null(cols)) {
    choices <- cols %>%
      dplyr::select(
        .data$choice_id, .data$choice_text,
        choice_position = .data$position, .data$col_id
      )
    cols <- cols %>%
      dplyr::select(-.data$choice_id, -.data$choice_text, -.data$position) %>%
      dplyr::distinct(.keep_all = TRUE)
  } else { # otherwise get choices from the other place they live
    choices <- parse_choices(ques)
  }

  # join them
  # then will join with responses on unique ID (Q/row/col) & choice_id
  out <- q_info
  if (!is.null(rows)) {
    out <- merge(out, rows)
  }
  if (!is.null(cols)) {
    out <- merge(out, cols)
  }
  if (!is.null(choices)) {
    out <- merge(out, choices)
  }
  if (!is.null(other)) {
    out <- dplyr::bind_rows(out, other) %>%
      tidyr::fill(.data$heading:.data$question_subtype)
  }

  tibble::as_tibble(out)
}

## These functions below are called by parse_question_info

#' @importFrom rlang .data
# Takes one col, returns data.frame
parse_cols <- function(ques) {
  #' @importFrom rlang .data
  get_single_col_info <- function(col) {
    dplyr::bind_rows(col$choices) %>%
      dplyr::rename(choice_id = .data$id, choice_text = .data$text) %>%
      dplyr::mutate(
        col_id = col$id,
        col_text = col$text
      ) %>%
      dplyr::select(-.data$visible, -.data$is_na)
  }
  if (!is.null(ques$answers$cols)) {
    cols <- purrr::map_df(ques$answers$cols, get_single_col_info)
  } else {
    cols <- NULL
  }
  cols
}

#' @importFrom rlang .data
parse_rows <- function(question) {
  if (!is.null(question$answers$rows)) {
    rows <- dplyr::bind_rows(question$answers$rows) %>%
      dplyr::rename(row_id = .data$id, row_text = .data$text) %>%
      dplyr::select(-.data$visible)
  } else {
    rows <- NULL
  }
  rows
}

#' @importFrom rlang .data
parse_choices <- function(question) {
  if (!is.null(question$answers$choices)) {
    choices <- dplyr::bind_rows(question$answers$choices) %>%
      dplyr::rename(
        choice_id = .data$id, choice_text = .data$text,
        choice_position = .data$position
      ) %>%
      dplyr::select(-.data$visible)
    choices$is_na <- NULL # won't always exist, remove if it does
  } else {
    choices <- NULL
  }
  choices
}

#' @importFrom rlang .data
parse_other <- function(question) {
  if (!is.null(question$answers$other)) {
    other <- dplyr::bind_rows(question$answers$other) %>%
      dplyr::rename(other_id = .data$id, other_text = .data$text) %>%
      # don't think we'll need columns besides these
      dplyr::select(.data$other_id, .data$other_text)
  } else {
    other <- NULL
  }
  other
}
