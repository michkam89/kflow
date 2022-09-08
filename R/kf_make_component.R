#' Make Kubeflow component from a function
#'
#' Given a defined R function export a YAML file for Kubeflow to consume.
#'
#' Names of metrics and ui metadata arguments are renamed to required
#' specifications. Kubeflow likes arguments to be typed, which R doesn't handle
#' naturally. This function looks for a slug at the end of each argument to
#' determine the type. For example, `table_name_string` would be considered an
#' input String. Output paths are identified by ending in `_out`. Supported
#' translations are:
#'
#' Inputs
#' * `_string` = String
#' * `_int` = Integer
#' * `_bool` = Bool
#' * `_float` = Float
#' * #TODO add support for List and Dict
#'
#' Outputs
#' * `_out` = outputPath
#' * `_metrics` = Metrics
#' * `_uimeta` = UI_metadata
#'
#' @param rfunction Name of target function.
#' @param name Name of component.
#' @param description Description of component.
#' @param image Location of Docker image.
#' @param file Location to write yaml. Optional.
#'
#' @return Component YAML
#' @export
#'
#' @examples
#' tfun <- function(table_string, pred_count_int, path_metrics) 2 * 2
#' kf_make_component("tfun", "Test Component", "Test out the component", "gcr.io/test/test")
kf_make_component <- function(rfunction, name, description, image, file) {
  # Create base YAML
  yaml_base <-
    list(
      name = name,
      description = description,
      inputs = list(),
      outputs = list(),
      implementation = list(
        container = list(
          image = image,
          args = list(),
          command = list()
        )
      )
    )

  # Parse Function Name
  #rfun <- stringr::str_split(rfunction, "::", simplify = TRUE)

  # Parse out Input/Output args
  fun_args <- formals(rfunction)

  input_args <- fun_args[grep(x = names(fun_args), pattern = "(_string|_path|_int|_bool|_float)$")]

  #input_args <- purrr::keep(fun_args, stringr::str_ends, pattern = "_string|_stringPath|_int|_bool|_float")

  output_args <- fun_args[grep(x = names(fun_args), pattern = "(_out|_metrics|_uimeta)$")]

  #output_args <- purrr::keep(fun_args, stringr::str_ends, pattern = "_out|_metrics|_uimeta")


  # Write Input/Output args
  yaml_base$inputs <- purrr::map2(
    names(input_args),
    input_args,
    ~ list(
      name = .x,
      type = find_kf_type(.x),
      default = find_arg_defaults(arg_name = .x, arg_value = .y),
      optional = is_arg_optional(.y)
      )
    )
  # %>% purrr::map(~purrr::modify_if(.x = ., .p = is.na, .f = as.null))

  yaml_base$outputs <- purrr::map(
    names(output_args),
    ~list(
      name = name_fixer(.),
      type = find_kf_type(.)
      )
    )

  # Rename metrics/ui outputs

  # Write Implementation Args
  yaml_base$implementation$container$args <- purrr::map(
    names(fun_args),
    ~as.list(
      setNames(
        object = name_fixer(.),
        nm = find_arg_type(.)
        )
      )
    )


  # Write Function Call
  arg_calls <- paste0("args[", 1:length(fun_args), "]", collapse = ",")

  yaml_base$implementation$container$command <- list(
    "Rscript",
    "-e", 'args<-commandArgs(trailingOnly=TRUE)',
    "-e", paste0(rfunction, '(', arg_calls, ")")
  )

  if (missing(file)) {
    yaml::as.yaml(yaml_base) |> cat(sep = "\n")
    return(yaml::as.yaml(yaml_base))
  }

  yaml::write_yaml(yaml_base, file)
}
