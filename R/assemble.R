#
# surv_id <- 168893066 # for dev't purposes, the Spring 2019 LIFT Teacher Survey
# one_surv <- fetch_survey_obj(surv_id)

# Bring it all together
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

  x <- dplyr::inner_join(responses, question_combos)
  assertthat::assert_that(nrow(x) == nrow(responses))

  # TODO - guarantee here that if key columns were missing from all question types, they are forced into existence


  ## At that point, if question type = Multiple Choice, include choice text + ID in the combined new columns

  ### Move this to after the merge with responses but here's a start...
  x$q_unique_id <- apply(
    x %>%
      dplyr::select(question_id, row_id, col_id, other_id),
    1,
    function(x) paste(na.omit(x), collapse="_")
  )
  x$q_unique_id[x$question_type == "multiple_choice" & is.na(x$other_id)] <- paste(
    x$q_unique_id[x$question_type == "multiple_choice" & is.na(x$other_id)],
    x$choice_id[x$question_type == "multiple_choice" & is.na(x$other_id)],
    sep = "_")

  x$combined_q_text <- apply(
    x %>%
      dplyr::select(heading, row_text, col_text, other_text),
    1,
    function(x) paste(na.omit(x), collapse= " - ")
  )
  x$combined_q_text[x$question_type == "multiple_choice" & is.na(x$other_text)] <- paste(
    x$combined_q_text[x$question_type == "multiple_choice" & is.na(x$other_text)],
    x$choice_text[x$question_type == "multiple_choice" & is.na(x$other_text)],
    sep = " - ")

  # combine open-response text and choice text into a single field to populate the eventual table
  x$answer <- dplyr::coalesce(x$response_text, x$choice_text)
  assertthat::assert_that(sum(!is.na(x$answer)) == (sum(!is.na(x$response_text)) + sum(!is.na(x$choice_text))))

  ## Pick up here; do I need this select()?
  x %>%
    dplyr::select(collector_id, recipient_id, response_id, question_type, combined_text, text, open_response_text, unique_q_id)


  qid_text_crosswalk <- final_x %>%
    dplyr::distinct(unique_q_id, .keep_all = TRUE) %>%
    dplyr::select(unique_q_id, combined_text) %>%
    dplyr::mutate(unique_text = de_duplicate_names(combined_text))

  # need a single blank response for each unique Q ID to spread - but more than that will cause spread to choke
  final_x_real <- final_x %>%
    dplyr::filter(!is.na(response_id))
  final_x_dummy <- final_x %>%
    dplyr::filter(is.na(response_id)) %>%
    dplyr::distinct(unique_q_id, .keep_all = TRUE)

  final_x <- dplyr::bind_rows(final_x_real, final_x_dummy) %>%
    dplyr::select(-question_type, -open_response_text) # remove for spread

  # spread wide
  # get column order to reset to after spread makes alphabetical
  col_names <- c(names(final_x)[!(names(final_x) %in% c("combined_text","text", "unique_q_id"))], qid_text_crosswalk$unique_text)

  out <- final_x %>%
    dplyr::select(-combined_text) %>%
    dplyr::mutate(unique_q_id = factor(unique_q_id, levels = qid_text_crosswalk$unique_q_id)) %>% # to spread unrepresented levels
    tidyr::pivot_wider(names_from = unique_q_id, values_from = text) %>%
     dplyr::filter(!is.na(response_id))

  # Takes spread-out results data.frame and turns multiple choice cols into factors.  GH issue #12
  # Doing this within the main function so it can see crosswalk
    master_qs <- dplyr::inner_join(choices, questions) %>%
      dplyr::mutate(combined_text = dplyr::case_when(
        !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
        position == 0 & question_type != "open_ended" ~ paste(heading, text, sep = " - "), # for "Other (please specify)" - I think position = 0 is consistent?
        question_type %in% "multiple_choice" ~ paste(heading, text, sep = " - "),
        TRUE ~ heading)) %>%
      dplyr::mutate(unique_q_id = dplyr::case_when( # spread and join on this as it works for duplicated Q text
        !is.na(subquestion_text) ~ paste(question_id, subquestion_id, sep = "_"),
        position == 0 & question_type != "open_ended" ~ paste(question_id, choice_id, sep = "_"), # for "Other (please specify)"
        question_type %in% "multiple_choice" ~ paste(question_id, choice_id, sep = "_"), # expand multiple choice Qs to be one-column-per
        TRUE ~ question_id
      ))

    # set a vector as a factor, if it has answer choices associated with its question id
    set_factor_levels <- function(vec, q_id){

      # fetch possible answer choices given a question's text
      get_factor_levels <- function(q_id){
        master_qs %>%
          dplyr::filter(unique_q_id == q_id) %>%
          dplyr::arrange(position) %>% # appears to always come from API in order but don't want to assume
          dplyr::pull(text) %>%
          unique() # this is not proper, remove it when the real fix to #20 comes in
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
  names(out)[4:length(names(out))] <- qid_text_crosswalk$unique_text[match(names(out)[4:length(names(out))],qid_text_crosswalk$unique_q_id)]
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
