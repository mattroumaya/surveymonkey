test_that("format_date() works", {
  src = as.Date("2022-01-09")
  posix_src = as.POSIXct(src)
  char_src = "2022-01-09 00:00:00"
  expected = "2022-01-09 00:00:00"
  expect_type(format_date(src), "character")
  expect_type(format_date(posix_src), "character")
  expect_type(format_date(char_src), "character")
  expect_equal(format_date(src), expected)
  expect_equal(format_date(posix_src), expected)
  expect_equal(format_date(char_src), expected)
})
