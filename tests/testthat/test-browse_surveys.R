with_mock_api({
  test_that("there is an error message when no token exists", {
    expect_error(browse_surveys(include = 'everything'))
  })
})
