read_nm_derivative_results <- function(lst_file, problem = NULL){
  if(!require("xpose", quietly = TRUE)) stop("The xpose package needs to be installed to create VA plots for NONMEM.")
  xpdb <- xpose::xpose_data(file = lst_file, simtab = FALSE, extra_files = c('.ext'))
  nm_output <- list()
  nm_output$thetavec <- get_theta_vector(xpdb, problem = problem)
  nm_output$iivmat <- get_omega_matrix(xpdb, problem = problem)
  nm_output$ruvmat <- get_sigma_matrix(xpdb, problem = problem)
  nm_output$derivdata <- get_table_data(xpdb,  problem = problem)
  return(nm_output)
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

