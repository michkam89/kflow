find_kf_type <- function(x) {
  x <- stringr::str_extract(x, "[^_]+$")

  lookup <- list(
    string = "String",
    stringPath = "String",
    int = "Integer",
    float = "Float",
    bool = "Bool",
    metrics = "Metrics",
    uimeta = "UI_metadata"
  )

  lookup[[x]]
}

find_arg_type <- function(x) {
  x <- stringr::str_extract(x, "[^_]+$") |>
    tibble::as_tibble()

  x |>
    dplyr::mutate(
      arg_type = dplyr::case_when(
        value %in% c("string", "int", "float", "bool") ~ "inputValue",
        value == "stringPath" ~ "inputPath",
        TRUE ~ "outputPath"
      )
    ) |>
    dplyr::pull(arg_type)
}

find_arg_defaults <- function(x) {
  if (is.symbol(x)) {
    rlang::as_string(x)
  } else {
    x
  }

}

is_arg_optional <- function(x) {
  ifelse(is.null(x), "yes", "no")
}
