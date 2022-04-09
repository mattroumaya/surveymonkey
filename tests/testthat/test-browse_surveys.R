test_that("there is an error message when no token exists", {
    expect_error(browse_surveys(include = 'everything', oauth_token = "foo"))
})

with_mock_api({
  surveys <- browse_surveys(oauth_token = "temp",
                            verbose = FALSE)

  test_that("browse surveys works as intended", {
    expect_equal(names(surveys), c("title", "id", "url", "nickname"))
    expect_equal(surveys$title, c("my survey", "dev"))
    expect_true("data.frame" %in% class(surveys))
  })

})

with_mock_api({
  surveys <- browse_surveys(oauth_token = "temp",
                            verbose = FALSE,
                            include = "everything")

  test_that("include = everything returns all fields", {
    expect_equal(names(surveys), c("title", "id", "url", "nickname", "response_count", "date_created",
                                   "date_modified", "language", "question_count", "analyze_url",
                                   "preview"))
  })
})

with_mock_api({
  test_that("all NULL values are stopped", {
    expect_error(browse_surveys(oauth_token = "temp",
                                  page = NULL,
                                  per_page = NULL,
                                  sort_by = NULL,
                                  sort_order = NULL,
                                  start_modified_at = NULL,
                                  end_modified_at = NULL,
                                  title = NULL,
                                  include = NULL,
                                  folder_id = NULL))
  })
})



