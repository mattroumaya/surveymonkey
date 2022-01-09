
withr::local_options()

test_that("get_token returns NULL with no option set", {
  expect_null(get_token())
})

test_that("setting the token works", {
  val = "5678"
  set_token(val)
  expect_equal(getOption("sm_oauth_token"), val)
})

test_that("can retrieve token", {
  withr::with_options(
    list(sm_oauth_token = "1234"), {
      expect_equal(get_token(), "1234")
    }
  )
})

test_that("get bearer token", {
  expect_error(get_bearer_token())
  expect_equal(get_bearer_token("test"), "bearer test")
})
