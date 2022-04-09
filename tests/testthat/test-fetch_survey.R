with_mock_api({
  data <- fetch_survey_obj(318754279,
                           oauth_token = "temp",
                           verbose = FALSE)
  test_that("fetch_survey_obj works as intended", {
    expect_equal(data$title, "my survey")
    expect_equal(data$language, "en")
    expect_equal(data$folder_id, "0")
    expect_equal(data$question_count, 2)
    expect_equal(data$page_count, 1)
    expect_equal(data$response_count, 2)
    expect_type(data, "list")
  })

  test_that("fetch_survey_obj fails without id", {
    expect_error(fetch_survey_obj(oauth_token = "temp",
                                  verbose = FALSE))
  })

  test_that("fetch_survey_obj fails without oauth", {
    expect_error(fetch_survey_obj(318754279,
                                  verbose = FALSE,
                                  oauth_token = NULL))
  })
})


