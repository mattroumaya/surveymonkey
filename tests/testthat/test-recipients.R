with_mock_api({

  test_that("retrieving recipients fails when none available", {

    collector <- get_collectors(318754279, oauth_token = "temp") %>% suppressWarnings()
    expect_error(get_recipients(collector$id[1]))

  })

})
