#' Read results from a NM run
#'
#' @param lst_file Path to a NONMEM results file
#' @param problem Number of the $PROBLEM to use
#' @param column_specs Column specifications as produced by nm_column_specs()
#' @param column_mappers Derivative to variable mapping specification as produced by nm_column_mappers()
#'
#' @export
prepare_va_nm <- function(lst_file, problem = NULL,
                                       column_specs = nm_column_specs(),
                                       column_mappers = nm_column_mappers()){

  if(!requireNamespace("xpose", quietly = TRUE)) stop("The xpose package needs to be installed to create VA plots for NONMEM.")
  xpdb <- xpose::xpose_data(file = lst_file, simtab = FALSE, extra_files = c('.ext'), quiet = TRUE)

  # read estimates
  theta <- get_theta_vector(xpdb, problem = problem)
  omega <- get_omega_matrix(xpdb, problem = problem)
  sigma <- get_sigma_matrix(xpdb, problem = problem)
  neta <- NCOL(omega)
  neps <- NCOL(sigma)
  iiv_vars <- paste0("ETA", seq_len(neta))
  ruv_vars <- paste0("EPS", seq_len(neps))

  tab <- get_table_data(xpdb,  problem = problem)
  column_names <- colnames(tab)
  # try column selection in safe manner
  selection_results <- purrr::map(column_specs, purrr::safely(~tidyselect::vars_select(colnames(tab), !!.x)))
  # collect selection errors
  selection_errors <- selection_results %>%
    purrr::map("error") %>%
    purrr::compact()

  if(length(selection_errors) != 0) {
    failed_columns <- names(selection_errors) %>% paste(collapse = ",")
    ui_error(paste("Could not find the required column(s):", failed_columns),
             suggestions = c("Check the supplied column_specs argument",
                             "In NONMEM, check the $TABLE statement for missing columns",
                             "Verify the generated table file"))
  }



  # selection results
  column_groups <- selection_results %>%
    purrr::map("result")

  # all remaining columns
  other_columns <- column_names[!column_names %in% unlist(column_groups)]
  column_groups[["other"]] <- other_columns

  #column_groups <- purrr::map(column_specs, ~tidyselect::vars_select(colnames(tab), !!.x))
  id_column <- tab[[column_groups[["id"]]]]

  select_rows <- function(cols, group, df) {
    if(group == "id"){
      df[1, cols, drop = TRUE]
    }else if(group == "eta"){
      data.matrix(df[1, cols, drop = FALSE]) %>% t()
    }else if(group == "deta"){
      m <- data.matrix(df[, cols, drop = FALSE])
      # rename columns to match ETAs
      eta_indicies <- column_mappers$deta_mapper(colnames(m))
      colnames(m) <- paste0("ETA",eta_indicies)
      return(m)
    }else if(group == "deps"){
      m <- data.matrix(df[, cols, drop = FALSE])
      # rename columns to match ETAs
      eps_indicies <- column_mappers$deps_mapper(colnames(m))
      colnames(m) <- paste0("EPS",eps_indicies)
      return(m)
    }else if(group == "deps_deta"){
      data.matrix(df[,cols, drop = FALSE])
    }else{
      df[,cols,drop=FALSE]
    }
  }

  # split by ID and extract column groups
  res <- split(tab, id_column) %>%
    purrr::map(function(df) purrr::imap(column_groups, ~select_rows(.x, .y, df)))

  nmout <- list()
  nmout$colnames <- other_columns
  nmout$thetavec <- theta
  nmout$omega <- omega
  nmout$sigma <- sigma
  nmout$derivdata <- res

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

