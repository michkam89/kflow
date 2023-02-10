test_that("correctly identifies inputValues", {
  expect_true(find_arg_type("my_string") == "inputValue")
  expect_true(find_arg_type("my_int") == "inputValue")
  expect_true(find_arg_type("my_float") == "inputValue")
  expect_true(find_arg_type("my_bool") == "inputValue")
})

test_that("correctly identifies inputPath", {
  expect_true(find_arg_type("my_path") == "inputPath")
})

test_that("correctly identifies outputPath", {
  expect_true(find_arg_type("my_out") == "outputPath")
  expect_true(find_arg_type("my_metrics") == "outputPath")
  expect_true(find_arg_type("my_uimeta") == "outputPath")
})

test_that("error on unknown arg_type", {
  expect_error(find_arg_type("my_random_type"), "Unknown argument type")
})
