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

  # read & prepare estimates
  estimates <- get_final_frem_estimates(file.path(directory, frem_specs$ext_file))

  neta <- NCOL(estimates$omega)
  neps <- NCOL(estimates$sigma)
  eta_names <- glue::glue("ETA{i}", i = seq_len(neta))
  eps_names <- glue::glue("EPS{i}", i = seq_len(neps))
  tab_split <- split_table_data(all_data,
                                ignore_expr = rlang::quo(.data$FREMTYPE>0),
                                column_specs = column_specs,
                                column_mappers = column_mappers,
                                eta_names = eta_names,
                                eps_names = eps_names)



  frem_covariates <- get_frem_covariates(directory, frem_specs)
  nfremeta <- length(frem_covariates)

  variable_labels <- c(glue::glue("ETA({i})", i = seq_len(neta-nfremeta)), frem_covariates)
  variable_types <- c(rep("iiv-re", neta-nfremeta), rep("covariate", nfremeta))
  variable_names <- eta_names

  omega <- estimates$omega
  sigma <- estimates$sigma
  rownames(omega) <- colnames(omega) <- eta_names
  rownames(sigma) <- colnames(sigma) <- eps_names

  inp <- va_input(
    column_names = colnames(tab_split[[1]][["other"]]),
    theta = estimates$theta,
    omega = omega,
    sigma = sigma,
    derivative_data = tab_split,
    input_file = directory,
    variable_labels = variable_labels,
    variable_types = variable_types,
    variable_names = eta_names
  )

  return(inp)

}

default_frem_specs <- function(derivatives_table = "final_models/derivatives.tab",
                               ext_file = "final_models/model_4.ext",
                               frem_dataset = "frem_dataset.dta",
                               covariates_summary = "covariates_summary.csv"){
  return(
    list(
      derivatives_table = derivatives_table,
      ext_file = ext_file,
      frem_dataset = frem_dataset,
      covariates_summary = covariates_summary
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

get_frem_covariates <- function(directory, frem_specs = default_frem_specs()){
  path <- file.path(directory, frem_specs$covariates_summary)
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  file_content <- readLines(path, warn = FALSE)
  if(length(file_content)<2) rlang::cnd_signal(cnd_unexpected_file_format(path))

  cov_row <- scan(text = file_content[[2]], sep = ",",
                 what = character(), quiet = TRUE)
  if(length(cov_row)<2) rlang::cnd_signal(cnd_unexpected_file_format(path))
  return(cov_row[-1])
}
