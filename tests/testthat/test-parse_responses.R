
# This is the result of a survey called Sample Survey for R Package API Testing
# Managed by Dustin Pashouwer and Sam Firke
# It's what's produced by the line responses <- parsed_content$data in get_responses.R

responses_raw <- readRDS("test_responses_raw.rds")

# Loads the results of parse_respondent_list(responses) when it was running correctly
# Yes, this is tautological, but provides protection from breaking it going forward
# Would be good to eventually add more exhaustive coverage of question types

parsed_correctly <- readRDS("parsed_responses.rds")

test_that("responses parsed correctly", {
  test_parsed <- parse_respondent_list(responses_raw)

  for (i in seq_along(parsed_correctly)) {
    expect_equal(
      parsed_correctly[[i]],
      test_parsed[[i]],
      info = paste0("comparing column ", i)
    )
  }
})
