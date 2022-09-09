source(testthat::test_path("files", "test_functions.R"))

test_that("File is written", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun1",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))
})

test_that("file is written when function comes from loaded package", {

  tmp_dir <- tempdir()
  withr::defer(fs::dir_delete(tmp_dir))

  usethis::ui_silence(usethis::create_package(tmp_dir, open = FALSE))

  withr::with_dir(
    tmp_dir,
    {

      fun <- c("comp_fun1 <- function(location_string,location2_path,count_int,",
               "weight_float,flag_bool,path_metrics,path_uimeta,results_out) {",
               "2 * 2}")

      writeLines(fun, con = "R/foo.R", sep = "\n")

      devtools::load_all()
      kf_make_component(
        rfunction = "comp_fun1",
        name = "Test Function",
        description = "A function for testing",
        image = "docker/docker",
        file = "component.yaml"
      )

      expect_true(fs::file_exists("component.yaml"))
    })


})

##### TEST STRING INPUTS -------------------------------------------------------

test_that("'_string' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun2",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[1]]$name == "location_string")
  expect_true(got$inputs[[1]]$type == "String")
  expect_true(is.null(got$inputs[[1]]$default))
  expect_false(got$inputs[[1]]$optional)
  expect_true(got$implementation$container$args[[1]]$inputValue == "location_string")
})

test_that("'_string' with default value is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun2",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[2]]$name == "default_location_string")
  expect_true(got$inputs[[2]]$type == "String")
  expect_true(got$inputs[[2]]$default == "/home/user")
  expect_false(got$inputs[[2]]$optional)
  expect_true(got$implementation$container$args[[2]]$inputValue == "default_location_string")
})

test_that("optional '_string' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun2",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[3]]$name == "optional_location_string")
  expect_true(got$inputs[[3]]$type == "String")
  expect_true(is.null(got$inputs[[3]]$default))
  expect_true(got$inputs[[3]]$optional)
  expect_true(got$implementation$container$args[[3]]$inputValue == "optional_location_string")
})

##### TEST INTEGER INPUTS ------------------------------------------------------

test_that("'_int' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun3",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[1]]$name == "my_int")
  expect_true(got$inputs[[1]]$type == "Integer")
  expect_true(is.null(got$inputs[[1]]$default))
  expect_false(got$inputs[[1]]$optional)
  expect_true(got$implementation$container$args[[1]]$inputValue == "my_int")
})

test_that("'_int' with default value is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun3",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[2]]$name == "default_my_int")
  expect_true(got$inputs[[2]]$type == "Integer")
  expect_true(got$inputs[[2]]$default == 1L)
  expect_false(got$inputs[[2]]$optional)
  expect_true(got$implementation$container$args[[2]]$inputValue == "default_my_int")
})

test_that("optional '_int' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun3",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[3]]$name == "optional_my_int")
  expect_true(got$inputs[[3]]$type == "Integer")
  expect_true(is.null(got$inputs[[3]]$default))
  expect_true(got$inputs[[3]]$optional)
  expect_true(got$implementation$container$args[[3]]$inputValue == "optional_my_int")
})

test_that("default '_int' is parsed to integer even if provided as double", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun3",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[4]]$name == "default_not_my_int")
  expect_true(got$inputs[[4]]$type == "Integer")
  expect_true(got$inputs[[4]]$default == 1L)
  expect_false(got$inputs[[4]]$optional)
  expect_true(got$implementation$container$args[[4]]$inputValue == "default_not_my_int")
})

##### TEST FLOATING INPUTS -----------------------------------------------------

test_that("'_float' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun4",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[1]]$name == "my_float")
  expect_true(got$inputs[[1]]$type == "Float")
  expect_true(is.null(got$inputs[[1]]$default))
  expect_false(got$inputs[[1]]$optional)
  expect_true(got$implementation$container$args[[1]]$inputValue == "my_float")
})

test_that("'_float' with default value is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun4",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[2]]$name == "default_my_float")
  expect_true(got$inputs[[2]]$type == "Float")
  expect_true(got$inputs[[2]]$default == 1.5)
  expect_false(got$inputs[[2]]$optional)
  expect_true(got$implementation$container$args[[2]]$inputValue == "default_my_float")
})

test_that("optional '_float' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun4",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[3]]$name == "optional_my_float")
  expect_true(got$inputs[[3]]$type == "Float")
  expect_true(is.null(got$inputs[[3]]$default))
  expect_true(got$inputs[[3]]$optional)
  expect_true(got$implementation$container$args[[3]]$inputValue == "optional_my_float")
})

test_that("default '_float' is parsed to double even if provided as integer", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun4",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[4]]$name == "default_not_my_float")
  expect_true(got$inputs[[4]]$type == "Float")
  expect_true(got$inputs[[4]]$default == as.double(1))
  expect_false(got$inputs[[4]]$optional)
  expect_true(got$implementation$container$args[[4]]$inputValue == "default_not_my_float")
})

##### TEST BOOLEAN INPUTS ------------------------------------------------------

test_that("'_bool' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun5",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[1]]$name == "my_bool")
  expect_true(got$inputs[[1]]$type == "Bool")
  expect_true(is.null(got$inputs[[1]]$default))
  expect_false(got$inputs[[1]]$optional)
  expect_true(got$implementation$container$args[[1]]$inputValue == "my_bool")
})

test_that("'_bool' with default value is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun5",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[2]]$name == "default_my_bool")
  expect_true(got$inputs[[2]]$type == "Bool")
  expect_true(got$inputs[[2]]$default == TRUE)
  expect_false(got$inputs[[2]]$optional)
  expect_true(got$implementation$container$args[[2]]$inputValue == "default_my_bool")
})

test_that("optional '_bool' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun5",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[3]]$name == "optional_my_bool")
  expect_true(got$inputs[[3]]$type == "Bool")
  expect_true(is.null(got$inputs[[3]]$default))
  expect_true(got$inputs[[3]]$optional)
  expect_true(got$implementation$container$args[[3]]$inputValue == "optional_my_bool")
})

test_that("default '_bool' converts to NULL if default is not logical or 0/1", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun5",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$inputs[[4]]$name == "default_not_my_bool")
  expect_true(got$inputs[[4]]$type == "Bool")
  expect_true(is.null(got$inputs[[4]]$default))
  expect_false(got$inputs[[4]]$optional)
  expect_true(got$implementation$container$args[[4]]$inputValue == "default_not_my_bool")
})

##### TEST REGULAR OUTPUTS -----------------------------------------------------

test_that("'_out' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun6",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  expect_true(got$outputs[[1]]$name == "my_out")
  expect_true(is.null(got$outputs[[1]]$type))
  expect_true(got$implementation$container$args[[1]]$outputPath == "my_out")
})

##### TEST METRICS OUTPUTS -----------------------------------------------------

test_that("'_out' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun6",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  print(got)
  expect_true(got$outputs[[2]]$name == "mlpipeline_metrics")
  expect_true(got$outputs[[2]]$type == "Metrics")
  expect_true(got$implementation$container$args[[2]]$outputPath == "mlpipeline_metrics")
})

##### TEST UI METADATA OUTPUTS -------------------------------------------------

test_that("'_out' is parsed correctly", {

  tfile <- tempfile()

  kf_make_component(
    rfunction = "comp_fun6",
    name = "Test Function",
    description = "A function for testing",
    image = "docker/docker",
    file = tfile)

  expect_true(file.exists(tfile))

  got <- yaml::read_yaml(tfile)

  print(got)
  expect_true(got$outputs[[3]]$name == "mlpipeline_ui_metadata")
  expect_true(got$outputs[[3]]$type == "UI_metadata")
  expect_true(got$implementation$container$args[[3]]$outputPath == "mlpipeline_ui_metadata")
})
