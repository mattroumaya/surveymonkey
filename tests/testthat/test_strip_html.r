library(dplyr)
library(testthat)

test_that("all <> and values between are removed", {


  cols <- c("What is your favorite color?", "Keep  value")

  expect_equal(mtcars %>%
                 select("What is your <strong>favorite</strong> color?" = mpg,
                        "Keep <this> value" = cyl) %>%
                 strip_html() %>%
                 colnames(.),
               cols)
  })


test_that("ignore values are kept", {

  cols <- c("What is your favorite color?", "Keep <this> value")

  expect_equal(mtcars %>%
                 select("What is your <strong>favorite</strong> color?" = mpg,
                        "Keep <this> value" = cyl) %>%
                 strip_html(ignore = "this") %>%
                 colnames(.),
               cols)
})

test_that("warning when values are not found", {
  expect_warning(mtcars %>%
                   select("What is your <strong>favorite</strong> color?" = mpg,
                          "Keep <this> value" = cyl) %>%
                   strip_html(ignore = "not_in_these_columns_satan!") %>%
                   colnames(.),
                 paste0("None of your ignored values were found. All text between <> will be removed."),
                        fixed = T)
})



