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
