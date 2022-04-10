with_mock_api({

  test_that("all pages works as intended", {
    responses <- get_responses(318754279, oauth_token = "temp", all_pages = TRUE) %>% suppressWarnings()
    parsed <- parse_respondent_list(responses)
    expect_true("data.frame" %in% class(parsed))

  })
})

