library(testthat)

test_that("all <> and values between are removed", {
  cols <- c("What is your favorite color?", "Keep value")

  expect_equal(
    mtcars %>%
      dplyr::select(
        "What is your <strong>favorite</strong> color?" = .data$mpg,
        "Keep <this> value" = .data$cyl
      ) %>%
      strip_html() %>%
      colnames(.),
    cols
  )
})


test_that("ignore values are kept", {
  cols <- c("What is your favorite color?", "Keep <this> value")

  expect_equal(
    mtcars %>%
      dplyr::select(
        "What is your <strong>favorite</strong> color?" = .data$mpg,
        "Keep <this> value" = .data$cyl
      ) %>%
      strip_html(ignore = "this") %>%
      colnames(.),
    cols
  )
})

test_that("warning when values are not found", {
  expect_warning(mtcars %>%
    dplyr::select(
      "What is your <strong>favorite</strong> color?" = .data$mpg,
      "Keep <this> value" = .data$cyl
    ) %>%
    strip_html(ignore = "not_in_these_columns_satan!") %>%
    colnames(.),
  paste0("None of your ignored values were found. All text between <> will be removed."),
  fixed = TRUE
  )
})


test_that("trim_space == TRUE is working correctly", {
  cols <- c("What is your favorite color?", "Keep value")

  expect_equal(
    mtcars %>%
      dplyr::select(
        "What is your <strong>favorite</strong> color?" = .data$mpg,
        "Keep <this> value" = .data$cyl
      ) %>%
      strip_html(trim_space = TRUE) %>%
      colnames(.),
    cols
  )
})

test_that("trim_space == FALSE is working correctly", {
  cols <- c("What is your favorite color?", "Keep  value")

  expect_equal(
    mtcars %>%
      dplyr::select(
        "What is your <strong>favorite</strong> color?" = .data$mpg,
        "Keep <this> value" = .data$cyl
      ) %>%
      strip_html(trim_space = FALSE) %>%
      colnames(.),
    cols
  )
})

test_that("trim_space == TRUE removes whitespace when removed text is at the end of column name", {
  col <- "remove this"

  expect_equal(
    tibble::tibble("remove this <text>" = c(1, 2, 3)) %>%
      strip_html(trim_space = TRUE) %>%
      colnames(.),
    col
  )
})

test_that(
  "trim_space == TRUE removes whitespace when
  removed text is at the beginning of column name", {
  col <- "to remove"

  expect_equal(
    tibble::tibble("<text> to remove" = c(1, 2, 3)) %>%
      strip_html(trim_space = TRUE) %>%
      colnames(.),
    col
  )
})
