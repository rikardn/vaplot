nm_column_specs <- function(id = "ID",
                            deta = matches("^G\\d+$"), deps = matches("^H\\d+$"),
                            deps_deta = matches("^D_EPSETA\\d+_\\d+_?$")){
  list(
    id = rlang::enquo(id),
    deta = rlang::enquo(deta),
    deps = rlang::enquo(deps),
    deps_deta = rlang::enquo(deps_deta)
  )
}

nm_column_mappers <- function(deta_mapper = function(x) extract_int(x, "(?<=G)\\d{2}"),
                              deps_mapper = function(x) extract_int(x, "(?<=H)\\d{2}")){
  list(
    deta_mapper = deta_mapper,
    deps_mapper = deps_mapper
  )
}

read_nm_table <- function(path){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  tab <- try(read.table(path, header = TRUE, skip = 1, colClasses = "numeric"), silent = TRUE)
  if(is_error(tab)) rlang::cnd_signal(cnd_unexpected_file_format(path))
  return(tab)
}



read_nm_final_ests <- function(path){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  file_content <- readLines(path, warn = FALSE)
  # find important rows
  intro_rows <- grepl("TABLE", file_content, fixed = TRUE)
  header_rows <- grepl("ITERATION", file_content, fixed = TRUE)
  final_est_rows <- grepl("  -1000000000", file_content, fixed = TRUE)
  if(!any(intro_rows) || !any(header_rows) || !any(final_est_rows))
    rlang::cnd_signal(cnd_unexpected_file_format(path))
  # parse header (assumes that all subsequent headers are identical)
  header <- scan(text = file_content[header_rows][1], what = character(), quiet = TRUE)
  ncols <- length(header)
  ntabs <- sum(intro_rows)
  # parse final estimate lines
  final_ests <- scan(text = file_content[final_est_rows], what = double(),  quiet = TRUE)
  names(final_ests) <- rep(header, times = ntabs)
  split(final_ests, rep(seq_len(ntabs), each = ncols))
}

extract_estimates <- function(ext_row_list){
  col_names <- names(ext_row_list[[1]])
  theta_cols <- grepl("THETA", col_names, fixed = TRUE)
  omega_cols <- grepl("OMEGA", col_names, fixed = TRUE)
  sigma_cols <- grepl("SIGMA", col_names, fixed = TRUE)
  purrr::map(ext_row_list, ~list(theta = .x[theta_cols],
                                 omega = lower_tri_vec_to_mat(.x[omega_cols]),
                                 sigma = lower_tri_vec_to_mat(.x[sigma_cols])))
}
