va_results <- function(table, ...){
  dots <- rlang::dots_list(...)
  column_specs <- dplyr::bind_rows(dots)
  return(
    structure(
      list(
        table = table,
        column_specs = column_specs
      ),
      class = 'va_results'
    ))
}

results_col <- function(name, type, variables = NULL, variable_types = NA_character_){
  variables <- list(variables)
  variable_types <- list(factor(variable_types, levels = c("covariate", "iiv-re", "ruv", NA_character_), exclude = NULL))
  return(
    list(
      name = name,
      type = factor(type, levels = c("idv", "variability", "facet-var")),
      variables = variables,
      variable_types = variable_types
    )
  )
}

get_variability_cols <- function(results){
  dplyr::filter(results$column_specs, .data$type == "variability") %>% dplyr::pull("name")
}

get_idv_col <- function(results){
  dplyr::filter(results$column_specs, .data$type == "idv") %>% dplyr::pull("name")
}

get_facet_cols <- function(results){
  dplyr::filter(results$column_specs, .data$type == "facet-var") %>% dplyr::pull("name")
}

get_cov_dependent_cols <- function(results){
  dplyr::filter(results$column_specs,
                .data$type == "variability",
                purrr::map_lgl(.data$variable_types, ~"covariate" %in% .x)) %>%
    dplyr::pull("name")
}

get_ruv_cols <- function(results){
  dplyr::filter(results$column_specs,
                .data$type == "variability",
                purrr::map_lgl(.data$variable_types, ~"ruv" %in% .x)) %>%
    dplyr::pull("name")
}

#' @export
print.va_results <- function(x, ...){
  cat("VA results from a linearized model\n")
  invisible(x)
}

