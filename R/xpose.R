get_theta_vector <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL) {
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>%
    dplyr::filter(ITERATION == -1000000000) %>%
    dplyr::select(dplyr::starts_with("THETA")) %>%
    unlist()
}

get_sigma_matrix <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL){
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>%
    dplyr::filter(ITERATION==-1000000000) %>%
    dplyr::select(dplyr::starts_with("SIGMA")) %>% unlist() %>%
    lower_tri_vec_to_mat()
}

