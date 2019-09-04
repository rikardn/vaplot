nm_column_specs <- function(id = ID, time = TIME, eta = matches("^ETA?\\d+$"),
                            deta = matches("^G\\d+$"), deps = matches("^H\\d+$"),
                            deps_deta = matches("^D_EPSETA\\d+_\\d+$")){
  list(
    id = rlang::enquo(id),
    time = rlang::enquo(time),
    eta = rlang::enquo(eta),
    deta = rlang::enquo(deta),
    deps = rlang::enquo(deps),
    deps_deta = rlang::enquo(deps_deta)
  )
}
