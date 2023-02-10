test_that("works for logicals", {
  got <- fix_bool_args(TRUE)
  want <- TRUE
  class(want) <- "verbatim"

  expect_equal(got, want)
})

test_that("works for integers", {
  got <- fix_bool_args(1)
  want <- TRUE
  class(want) <- "verbatim"

  expect_equal(got, want)
})

test_that("TRUE for any number that is not 0/1 - default R behaviour", {
  got <- fix_bool_args(1.5)
  want <- TRUE
  class(want) <- "verbatim"

  expect_equal(got, want)
})

test_that("NULL for text", {
  got <- fix_bool_args("text")
  want <- NULL

  expect_equal(got, want)
})
