test_that("standard request header", {
  expect_error(standard_request_header(NULL))
  expected = c(
    Authorization = "bearer 1234",
    "Content-Type" = "application/json"
  )
  test_token = "1234"
  h = standard_request_header(test_token)
  expect_s3_class(h, "request")
  expect_true(all(expected %in% h$headers))
})

test_that("response messages", {
  fake_response1 = list(
    headers = list(
      # note this is different to documented name
      `x-ratelimit-app-global-day-remaining` =  21,
      `X-Ratelimit-App-Global-Day-Reset` = 3600
    )
  )
  fake_response2 = list(
    headers = list(
      # note this is different to documented name
      `x-ratelimit-app-global-day-remaining` =  20,
      `X-Ratelimit-App-Global-Day-Reset` = 3600
    )
  )
  expect_message(
    remaining_request_message(fake_response1),
    "You have 21 requests left today before you hit the limit"
  )
  expect_null(
    reset_time_message(fake_response1)
  )
  expect_message(
    reset_time_message(fake_response1, 7),
    "Your daily request limit will reset in 3600 seconds"
  )
  expect_message(
    reset_time_message(fake_response2),
    "Your daily request limit will reset in 3600 seconds"
  )
})

# TODO: add test for full sm_get
# needs a test request set with associated key
