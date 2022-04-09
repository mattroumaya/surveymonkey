test_that("standard request header is created as expected", {
  header <- standard_request_header(token = "abcX123")

  expect_equal(header$headers[[1]], "bearer abcX123")
  expect_equal(header$headers[[2]], "application/json")
})

test_that("standard request header requires token", {
  expect_error(standard_request_header())
})

