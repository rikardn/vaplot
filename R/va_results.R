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

combine_results <- function(...){
  res_list <- rlang::list2(...)
  # extract all variable names
  var_names <- purrr::map(res_list, get_all_cols) %>% purrr::flatten_chr() %>% unique()
  # check that IDVs are identical
  idvs <- purrr::map_chr(res_list, get_idv_col) %>% unique()
  if(length(idvs)!=1) ui_error("Results need to have the same IDV to be combinable",
                               suggestions = c("Ensure that the same specifications are used when preparing the VA input"))

  # extract types
  types <- purrr::map(res_list, get_types_chr) %>%
    purrr::reduce(~purrr::list_merge(.x, !!!.y)) %>%
    purrr::map(unique)

  # check that types are consistent
  if(any(purrr::map_int(types,length)>1)) ui_error("Results need to have consistent column types",
                                                   suggestions = c("Ensure that the same specifications are used when preparing the VA input"))

  # merge variable types
  var_types <- purrr::map(res_list, get_variable_types_chr) %>%
    purrr::reduce(~purrr::list_merge(.x, !!!.y)) %>%
    purrr::map(factor, levels = c("covariate", "iiv-re", "ruv"))

  # rename variables and merge
  suffix_if_not_na <- function(x, s) ifelse(is.na(x), x, paste0(x, "_r", s))
  vars <- purrr::map2(res_list, seq_along(res_list), ~purrr::map(get_variables(.x), suffix_if_not_na, s = .y)) %>%
    purrr::reduce(~purrr::list_merge(.x, !!!.y)) %>%
    purrr::map(unique)

  column_specs <- dplyr::tibble(name = var_names,
                                 type = types[var_names] %>%
                                   purrr::flatten_chr() %>%
                                   factor(levels = c("idv", "variability", "facet-var")),
                                 variables = vars[var_names]%>% purrr::set_names(NULL) ,
                                 variable_types = var_types[var_names] %>% purrr::set_names(NULL)) %>%
    dplyr::add_row(!!!results_col(".result", type = "facet-var"))
  variability_cols <- dplyr::filter(column_specs, .data$type == "variability") %>% dplyr::pull("name")
  table <- purrr::map(res_list, "table") %>%
    dplyr::bind_rows(.id = ".result") %>%
    dplyr::mutate(
      .result = factor(.data$.result, levels = names(res_list))
    ) %>%
    dplyr::mutate_at(variability_cols, ~ifelse(is.na(.), 0, .))
  return(
    structure(
      list(
        column_specs = column_specs,
        table = table
      ),
      class = "va_results"
    )
  )
}

results_col <- function(name, type, variables = NULL, variable_types = NULL){
  variables <- if(is.null(variables)) NA_character_ else variables
  variable_types <- if(is.null(variable_types)) NA_character_ else variable_types
  variables <- list(variables)
  variable_types <- list(factor(variable_types, levels = c("covariate", "iiv-re", "ruv")))
  return(
    list(
      name = name,
      type = factor(type, levels = c("idv", "variability", "facet-var")),
      variables = variables,
      variable_types = variable_types
    )
  )
}

get_all_cols <- function(results){
  return(results$column_specs$name)
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

get_types <- function(results){
  purrr::set_names(as.list(results$column_specs$type),
                   results$column_specs$name)
}

get_types_chr <- function(results){
  purrr::set_names(purrr::map(results$column_specs$type, as.character),
                   results$column_specs$name)
}

get_variable_types <- function(results){
  purrr::set_names(results$column_specs$variable_types,
                   results$column_specs$name)
}

get_variable_types_chr <- function(results){
  purrr::set_names(purrr::map(results$column_specs$variable_types, as.character),
                   results$column_specs$name)
}

get_variables <- function(results){
  purrr::set_names(results$column_specs$variables,
                   results$column_specs$name)
}

#' @export
print.va_results <- function(x, ...){
  cat("VA results from a linearized model\n")
  invisible(x)
}

