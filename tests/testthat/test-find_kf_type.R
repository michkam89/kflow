test_that("correctly finds strings", {
  got <- find_kf_type("my_string")
  want <- "String"

  expect_equal(got, want)
})

test_that("correctly finds paths as strings", {
  got <- find_kf_type("my_path")
  want <- "String"

  expect_equal(got, want)
})

test_that("correctly finds int", {
  got <- find_kf_type("my_int")
  want <- "Integer"

  expect_equal(got, want)
})

test_that("correctly finds floats", {
  got <- find_kf_type("my_float")
  want <- "Float"

  expect_equal(got, want)
})

test_that("correctly finds bools", {
  got <- find_kf_type("my_bool")
  want <- "Bool"

  expect_equal(got, want)
})

test_that("correctly finds metrics", {
  got <- find_kf_type("my_metrics")
  want <- "Metrics"

  expect_equal(got, want)
})

test_that("correctly finds UI metadata", {
  got <- find_kf_type("my_uimeta")
  want <- "UI_metadata"

  expect_equal(got, want)
})

test_that("ignores '_out'", {
  got <- find_kf_type("my_out")
  want <- NULL

  expect_equal(got, want)
})

test_that("ignores all other suffixes", {
  got <- find_kf_type("my_random_suffix")
  want <- NULL

  expect_equal(got, want)
})
