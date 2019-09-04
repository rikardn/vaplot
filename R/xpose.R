read_nm_derivative_results <- function(lst_file, problem = NULL, column_specs = nm_column_specs()){
  if(!requireNamespace("xpose", quietly = TRUE)) stop("The xpose package needs to be installed to create VA plots for NONMEM.")
  xpdb <- xpose::xpose_data(file = lst_file, simtab = FALSE, extra_files = c('.ext'))
  tab <- get_table_data(xpdb,  problem = problem)

  if(length(tidyselect::vars_select(colnames(tab), !!column_specs$deta))==0) stop("No columns with derivative data found.")

  column_groups <- purrr::map(column_specs, ~tidyselect::vars_select(colnames(tab), !!.x))
  id_column <- tab[[column_groups[["id"]]]]

  select_rows <- function(cols, group, df) {
    if(group == "id"){
      df[1, cols, drop = TRUE]
    }else if(group == "eta"){
      data.matrix(df[1, cols, drop = FALSE]) %>% t()
    }else{
      data.matrix(df[,cols, drop = FALSE])
    }
  }

  # split by ID and extract column groups
  res <- split(tab, id_column) %>%
    purrr::map(function(df) purrr::imap(column_groups, ~select_rows(.x, .y, df)))

  nmout <- list()
  nmout$thetavec <- get_theta_vector(xpdb, problem = problem)
  nmout$omega <- get_omega_matrix(xpdb, problem = problem)
  nmout$sigma <- get_sigma_matrix(xpdb, problem = problem)
  nmout$derivdata <- res
  iiv_vars <- paste0("ETA", seq_len(NCOL(nmout$omega)))
  ruv_vars <- paste0("EPS", seq_len(NCOL(nmout$sigma)))
  rownames(nmout$omega) <- colnames(nmout$omega) <- iiv_vars
  rownames(nmout$sigma) <- colnames(nmout$sigma) <- ruv_vars


  return(nmout)
}

get_table_data <- function(xpdb, problem = NULL){
  xpose::get_data(xpdb, .problem = problem, quiet = TRUE)
}

get_theta_vector <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL) {
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>%
    dplyr::filter(ITERATION == -1000000000) %>%
    dplyr::select(dplyr::starts_with("THETA")) %>%
    unlist()
}

get_omega_matrix <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL){
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>%
    dplyr::filter(ITERATION==-1000000000) %>%
    dplyr::select(dplyr::starts_with("OMEGA")) %>% unlist() %>%
    lower_tri_vec_to_mat()
}

get_sigma_matrix <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL){
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>%
    dplyr::filter(ITERATION==-1000000000) %>%
    dplyr::select(dplyr::starts_with("SIGMA")) %>% unlist() %>%
    lower_tri_vec_to_mat()
}

