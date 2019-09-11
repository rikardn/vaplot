var_calc_lf <- function(linobj, conditioning_order, idv, facets){

  idv_var <- try(tidyselect::vars_select(linobj$colnames, !!idv), silent = TRUE)

  if(is_error(idv_var)) {
    ui_error("Could not select idv column",
             suggestions = c(paste0("Ensure the idv expression is correct"),
                             "In NONMEM, check the $TABLE statement for missing columns",
                             "Verify the generated table file"))
  }
  if(length(idv_var)>1) {
    ui_error(paste0("idv can only be one column (now it was ", length(idv_var), ")"),
             suggestions = c(paste0("Ensure the idv expression is correct")))
  }

  if(!rlang::quo_is_null(facets)){
    facet_vars <- try(tidyselect::vars_select(linobj$colnames, !!facets), silent = TRUE)
    if(is_error(facet_vars)) {
      ui_error("Could not select facet column(s)",
               suggestions = c(paste0("Ensure the facet expression is correct"),
                               "In NONMEM, check the $TABLE statement for missing columns",
                               "Verify the generated table file"))
    }
  }else{
    facet_vars <- NULL
  }


  cond_args_list <- gen_cond_args(conditioning_order)
  call_var_calc <- function(deta, omega, cond_args) purrr::exec(var_iiv_from_cond_lf,
                                                                deta = deta,
                                                                omega = omega,
                                                                !!!cond_args)
  va_table <- purrr::map_df(linobj$derivdata,
                ~purrr::map(.x = cond_args_list,
                            .f = call_var_calc,
                            omega = linobj$omega,
                            deta = .x$deta) %>%
                  purrr::update_list(RUV = var_ruv_lf(.x$deps, .x$deps_deta, linobj$omega, linobj$sigma)) %>%
                  dplyr::as_tibble() %>%
                  dplyr::bind_cols(dplyr::select(.x$other, idv_var, facet_vars))) %>%
    dplyr::group_by_at(c(facet_vars, idv_var)) %>%
    dplyr::summarise_all(mean)

  column_names <- colnames(va_table)
  column_types <- rep("", length(column_names))
  names(column_types) <- column_names
  column_types[column_names==idv_var] <- "idv"
  column_types[column_names %in% facet_vars] <- "facet_var"
  column_types[column_types==""] <- "variability"
  va_res <- va_results(
    table = va_table,
    column_types = column_types
  )
  return(va_res)
}

gen_cond_args <- function(conditioning_order){
   purrr::accumulate(conditioning_order, ~list(vars = .y, cond_on = unlist(.x, use.names = F)), .init = list()) %>%
    .[-1]
}


# calculates the variability from the RUV model for linearized model
var_ruv_lf <- function(deps, deps_deta, omega, sigma){
  interaction_terms <- diag(deps_deta %*% kronecker(omega, sigma) %*% t(deps_deta))
  diag(deps %*% sigma %*% t(deps))+ interaction_terms
}

#
# calc_variable_contribution <- function(linobj, phi = NULL, covariates = NULL, conditioning_order) {
#
#   if(is.null(phi)&&!rlang::is_empty(covariates)) stop("list of phi expressions needs to be supplied to handle covariates.")
#   if(!is.null(phi))  dphi <- calc_phi_derivatives(xpdb, phi, covariates)
#
#   omega_cov_matrix <- get_omega_cov_matrix(xpdb, covariates)
#
#   conditioning_args <- conditioning_order %>%
#     purrr::accumulate(~list(vars = .y, cond_on = unlist(.x, use.names = F)), .init = c()) %>%
#     .[-1] # remove first entry
#
#   purrr::modify(linobj$derivdata,
#                 ~ purrr::update_list(.,
#
#   lin_data %>%
#     xpose::fetch_data(filter = xpose::only_obs(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
#     dplyr::select(ID, dplyr::matches("G\\d{3}")) %>%
#     dplyr::rename_at(dplyr::vars(-ID), dplyr::funs(stringr::str_extract(., "(?<=G)\\d{2}") %>% as.integer() %>% as.character())) %>%  # rename columns to eta index
#     tidyr::nest(-ID, .key = "df_deta") %>%
#     dplyr::mutate(
#       df_deta = purrr::map(df_deta, as.matrix)) %>%
#     {
#       if(!is.null(phi)){
#         dplyr::left_join(., dphi, by = "ID") %>%
#           mutate(
#             df_da  = purrr::pmap(list(df_deta, dphi_deta, dphi_da), ~..1 %*% solve(..2) %*% ..3),
#             df_dvar = purrr::map2(df_deta, df_da, cbind)
#           )
#       }else{
#         mutate(., df_dvar = df_deta)
#       }
#     } %>%
#     dplyr::mutate(
#       eta_contribution =  purrr::map(df_dvar,
#                                      ~purrr::invoke_map(variability_from_vars_given_vars, conditioning_args, df_dvar = .x, omega_cov = omega_cov_matrix) %>%
#                                        purrr::set_names(col_names) %>%
#                                        dplyr::bind_cols())
#     )
# }

var_iiv_from_cond_lf <- function(deta, omega, vars, cond_on = c()){
  if(length(cond_on)==0) return(var_iiv_from_lf(deta, omega, vars))
  var_iiv_cond_lf(deta, omega, cond_on) - var_iiv_cond_lf(deta, omega, union(vars, cond_on))
}

var_iiv_cond_lf <- function(deta, omega, cond_on = c()){
  if(length(cond_on)==NROW(omega)) return(0)
  apply(deta, 1, function(a) t(dr(a, cond_on)) %*% schur_complement_named(omega, cond_on) %*% dr(a, cond_on))
}

var_iiv_from_lf <- function(deta, omega, vars){
  apply(deta, 1, function(a)  {
    if(!any(colnames(omega) %in% vars)) return(0)
    x <- sr(a, vars)+t(dr(a, vars))%*%dsrc(omega, vars, vars)%*%MASS::ginv(src(omega, vars))
    x %*% src(omega, vars) %*% t(x)
  })
}
