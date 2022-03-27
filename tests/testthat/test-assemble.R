no_dupes <- cars[1, ]
dupes <- rbind(no_dupes, no_dupes)

test_that("detect duplication", {
  expect_true(contains_duplicates(dupes))
  expect_false(contains_duplicates(no_dupes))
})

test_that("keep duplication", {
  expect_warning(res <- duplicate_keep(dupes))
  expect_identical(res, dupes)
  expect_warning(res <- duplicate_keep(no_dupes), NA) # expect no warning
  expect_identical(res, no_dupes)
})

test_that("drop duplication", {
  expect_warning(res <- duplicate_drop(dupes))
  expect_identical(res, no_dupes)
  expect_warning(res <- duplicate_keep(no_dupes), NA) # expect no warning
  expect_identical(res, no_dupes)
})

test_that("error on duplicates", {
  expect_error(duplicate_error(dupes))
  expect_error(duplicate_error(no_dupes), NA) # expect no error
})
