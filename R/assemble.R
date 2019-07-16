
#' Take a survey object and parses it into a tidy data.frame.
#'
#' @param surv_obj a survey, the result of a call to \code{fetch_survey_obj}.
#'
#' @return a data.frame (technically a \code{tibble}) with clean responses, one line per respondent.
#' @export

parse_survey <- function(surv_obj){
  respondents <- get_responses(surv_obj$id)
  responses <- respondents %>%
    parse_respondent_list()

  question_combos <- parse_all_questions(surv_obj)

  # this join order matters - putting q_combos on left yields the right ordering of columns in final result
  # the joining variables vary depending on question types present, so can't hard-code. Thus squash message
  x <- suppressMessages(dplyr::full_join(question_combos, responses))

  # There should not be duplicate rows here, but putting this here in case of oddities like #27
  assertthat::assert_that(sum(duplicated(dplyr::select_if(x, is.atomic))) == 0,
                          msg = "There are duplicated rows in the responses, maybe like #27 - file a bug report with the maintainer")

  #If question type = Multiple Choice, include choice text + ID in the combined new columns

  x$q_unique_id <- apply(
    x %>%
      dplyr::select(question_id, row_id, col_id, other_id),
    1,
    function(x) paste(stats::na.omit(x), collapse="_")
  )
  x$q_unique_id[x$question_type == "multiple_choice" & is.na(x$other_id)] <- paste(
    x$q_unique_id[x$question_type == "multiple_choice" & is.na(x$other_id)],
    x$choice_id[x$question_type == "multiple_choice" & is.na(x$other_id)],
    sep = "_")

  x$combined_q_heading <- apply(
    x %>%
      dplyr::select(heading, row_text, col_text, other_text),
    1,
    function(x) paste(stats::na.omit(x), collapse= " - ")
  )
  x$combined_q_heading[x$question_type == "multiple_choice" & is.na(x$other_text)] <- paste(
    x$combined_q_heading[x$question_type == "multiple_choice" & is.na(x$other_text)],
    x$choice_text[x$question_type == "multiple_choice" & is.na(x$other_text)],
    sep = " - ")

  # combine open-response text and choice text into a single field to populate the eventual table
  x$answer <- dplyr::coalesce(x$response_text, x$choice_text)
  assertthat::assert_that(sum(!is.na(x$answer)) == (sum(!is.na(x$response_text)) + sum(!is.na(x$choice_text))),
                          msg = "Uh oh, the maintainer failed to account for a combination of open-response text;
                          file a bug report")

  final_x <- x %>%
    dplyr::select(survey_id, collector_id, recipient_id, response_id, combined_q_heading, answer, q_unique_id)


  qid_text_crosswalk <- final_x %>%
    dplyr::distinct(q_unique_id, combined_q_heading) %>%
    dplyr::mutate(unique_text = de_duplicate_names(combined_q_heading))

  # did a full_join above to make sure that all questions [q_unique_ids] are present in result even if no one answered them
  # but that means the spread will fail b/c there's more than one response per q_unique_id for response_id == NA
  # Adjust for that to spread, then filter that out after spread
  final_x_real <- final_x %>%
    dplyr::filter(!is.na(response_id))

  final_x_dummy <- final_x %>%
    dplyr::filter(is.na(response_id)) %>%
    dplyr::distinct(q_unique_id)

  final_x <- dplyr::bind_rows(final_x_real, final_x_dummy)

  # spread wide
  # get column order to reset to after spread makes alphabetical
  col_names <- c(names(final_x)[!(names(final_x) %in% c("combined_q_heading","answer", "q_unique_id"))], qid_text_crosswalk$unique_text)

  out <- final_x %>%
    dplyr::select(-combined_q_heading) %>%
    dplyr::mutate(q_unique_id = factor(q_unique_id, levels = qid_text_crosswalk$q_unique_id)) %>% # to spread unrepresented levels
    tidyr::pivot_wider(names_from = q_unique_id, values_from = answer) %>%
    dplyr::filter(!is.na(response_id))

  # Takes spread-out results data.frame and turns multiple choice cols into factors.  GH issue #12
  # Doing this within the main function so it can see crosswalk
  master_qs <- x %>%
    dplyr::distinct(q_unique_id, choice_id, question_id, choice_position, choice_text)

  # set a vector as a factor, if it has answer choices associated with its question id
  set_factor_levels <- function(vec, q_id){

    # fetch possible answer choices given a question's text
    get_factor_levels <- function(q_id){
      master_qs %>%
        dplyr::filter(q_unique_id == q_id, !is.na(choice_id)) %>%
        dplyr::arrange(choice_position) %>% # appears to always come from API in order but don't want to assume
        dplyr::pull(choice_text)
    }

    name_set <- get_factor_levels(q_id)
    if(length(name_set) == 0){
      return(vec)
    } else {
      factor(vec, levels = name_set)
    }
  }
  out <- purrr::map2_dfc(out, names(out), set_factor_levels)

  # reset to text names instead of numbers
  # and then re-order to correct columns
  names(out)[5:length(names(out))] <- qid_text_crosswalk$unique_text[match(names(out)[5:length(names(out))],qid_text_crosswalk$q_unique_id)]
  out <- out[, col_names]
  out <- out %>% dplyr::arrange(desc(response_id))
  out
}

# Helper function for de-duplicating identical Q names
# Input: the vector of names
# Adapted from janitor::make_clean_names()
de_duplicate_names <- function(x){
  dupe_count <- vapply(seq_along(x), function(i) {
    sum(x[i] == x[1:i])
  }, integer(1))
  x[dupe_count > 1] <- paste(x[dupe_count >
                                 1], dupe_count[dupe_count > 1], sep = "_")
  x
}
