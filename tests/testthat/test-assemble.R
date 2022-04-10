with_mock_api({
  test_that("survey data is returned as expected", {
    survey <- fetch_survey_obj(318754279, oauth_token = "temp") %>% suppressWarnings()
    survey <- parse_survey(survey, oauth_token = "temp") %>% suppressWarnings()
    expect_equal(names(survey), c("survey_id", "collector_id", "respondent_id", "date_created",
                                  "date_modified", "response_status", "ip_address", "How many pets do you have?",
                                  "What are the names of your pets?"))
    expect_true("data.frame" %in% class(survey))
    expect_type(survey$survey_id, "double")
    expect_type(survey$collector_id, "character")
    expect_type(survey$respondent_id, "character")
    expect_true("POSIXct" %in% class(survey$date_created))
    expect_true("POSIXct" %in% class(survey$date_modified))
    expect_type(survey$response_status, "character")
    expect_type(survey$ip_address, "character")
    expect_true(is.factor(survey$`How many pets do you have?`))
    expect_true(is.character(survey$`What are the names of your pets?`))
    expect_true(!all(is.na(survey)))
  })
})

with_mock_api({
  test_that("response count == 0 shows a warning", {
    oauth <- "temp"
    survey <- fetch_survey_obj(318754279, oauth_token = oauth) %>% suppressWarnings()
    survey$response_count <- 0
    expect_warning( parse_survey(survey, oauth_token = "temp"))

  })
})


