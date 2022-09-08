name_fixer <- function(x) {
  x_stub <- stringr::str_extract(x, "[^_]+$")
  # When argument ends in "_metrics":
  if (x_stub == "metrics") {
    return("mlpipeline_metrics")
  }

  # When argument ends in "_uimeta":
  if (x_stub == "uimeta") {
    return("mlpipeline_ui_metadata")
  }

  x
}

find_kf_type <- function(x) {
  stopifnot(length(x) == 1)

  x <- stringr::str_extract(x, "[^_]+$")

  lookup <- list(
    string = "String",
    path = "String",
    int = "Integer",
    float = "Float",
    bool = "Bool",
    metrics = "Metrics",
    uimeta = "UI_metadata"
  )

  lookup[[x]]
}

find_arg_type <- function(x) {
  x <- stringr::str_extract(x, "[^_]+$")

  if (x %in% c("string", "int", "float", "bool")) {
    return("inputValue")
  } else if (x == "path") {
    return("inputPath")
  } else if (x %in% c("out", "metrics", "uimeta")) {
    return("outputPath")
  } else {
    stop("Unknown argument type")
  }
}

find_arg_defaults <- function(arg_name, arg_value) {
  # if default argument is function of some sort change it to character
  if (is.symbol(arg_value)) {

    if (rlang::as_string(arg_value) == "") {
      return(as.null(arg_value))

    } else {
      return(arg_value)
    }

  } else if (is.numeric(arg_value) & !grepl(x = arg_name, pattern = "_bool$")) {

    arg_value <- fix_int_args(arg_name, arg_value)

  } else if (grepl(x = arg_name, pattern = "_bool$")) {

    arg_value <- fix_bool_args(arg_value)
  }

  return(arg_value)
}

fix_bool_args <- function(arg_value) {
  x <- as.logical(arg_value)

  if (length(x) == 0 || is.na(x)) {
    y <- as.null(x)
    return(y)
  }

  class(x) <- "verbatim"
  return(x)
}

fix_int_args <- function(arg_name, arg_value) {
  if (grepl(x = arg_name, pattern = "_int$")) {
    return(as.integer(arg_value))
  } else {
    return(arg_value)
  }
}

is_arg_optional <- function(x) {
  x <- ifelse(is.null(x), TRUE, FALSE)
  class(x) <- "verbatim"
  return(x)
}
