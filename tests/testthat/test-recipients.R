with_mock_api({

  test_that("retrieving recipients fails when none available", {

    collector <- get_collectors(318754279,
                                verbose = FALSE,
                                oauth_token = "temp")
    expect_error(get_recipients(collector$id[1]))

  })

})
