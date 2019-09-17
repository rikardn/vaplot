#' Read results from a PsN FREM run
#'
#' @param directory
#' @param frem_specs
#'
#' @return
#' @export
prepare_va_frem <- function(directory,
                            frem_specs = default_frem_specs(),
                            column_specs = nm_column_specs(),
                            column_mappers = nm_column_mappers()){


  # get derivatives data & merge with frem data
  derivatives_tab <- read_nm_table(file.path(directory, frem_specs$derivatives_table))
  frem_data <- read_frem_data(file.path(directory, frem_specs$frem_dataset))
  frem_data <- dplyr::select(frem_data, -.data$ID) %>%
    dplyr::select(-!!column_specs$deps_deta, -!!column_specs$deps, -!!column_specs$deta)

  all_data <- dplyr::bind_cols(derivatives_tab, frem_data)

  tab_split <- split_table_data(all_data,
                                ignore_expr = rlang::quo(FALSE),
                                column_specs = column_specs,
                                column_mappers = column_mappers)


  # read & prepare estimates
  estimates <- get_final_frem_estimates(file.path(directory, frem_specs$ext_file))

  neta <- NCOL(estimates$omega)
  neps <- NCOL(estimates$sigma)
  iiv_vars <- paste0("ETA", seq_len(neta))
  ruv_vars <- paste0("EPS", seq_len(neps))
  omega <- estimates$omega
  sigma <- estimates$sigma
  rownames(omega) <- colnames(omega) <- iiv_vars
  rownames(sigma) <- colnames(sigma) <- ruv_vars

  inp <- va_input(
    column_names = colnames(tab_split[[1]][["other"]]),
    theta = estimates$theta,
    omega = omega,
    sigma = sigma,
    derivative_data = tab_split,
    input_file = directory
  )

  return(inp)

}

default_frem_specs <- function(final_model_output = "final_models/model_4.lst",
                               derivatives_table = "final_models/derivatives.tab",
                               ext_file = "final_models/model_4.ext",
                               frem_dataset = "frem_dataset.dta"){
  return(
    list(
      final_model_output = final_model_output,
      derivatives_table = derivatives_table,
      ext_file = ext_file,
      frem_dataset = frem_dataset
    )
  )
}

read_frem_data <- function(path){
  if(!file.exists(path)) rlang::signal(cnd_file_not_found(path))
  read.csv(path, header = TRUE, sep = ",", colClasses = "numeric")
}

get_final_frem_estimates <- function(path){
  read_nm_final_ests(path) %>%
    extract_estimates() %>%
    purrr::pluck(1)
}
