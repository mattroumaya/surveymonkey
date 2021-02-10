library(dplyr)
library(testthat)

survey <- dplyr::tibble("What is your <strong>favorite</strong> color?" = c("Red", "Blue", "Yellow"),
                        "Keep <this> value" = c(1,1,1))



test_that("all <> and values between are removed", {

  cols <- c("What is your favorite color?", "Keep  value")

  expect_equal(colnames(strip_html(survey)),
               cols)
  })


test_that("ignore values are kept", {

  cols <- c("What is your favorite color?", "Keep <this> value")

  expect_equal(colnames(strip_html(survey, ignore = "this")),
               cols)
})

test_that("warning when values are not found", {

  expect_warning(strip_html(survey, ignore = "not_in_these_columns_satan!"),
                 paste0("None of your ignored values were found. All text between <> will be removed."),
                        fixed = T)
})

