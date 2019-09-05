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

nm_column_mappers <- function(deta_mapper = function(x) extract_int(x, "(?<=G)\\d{2}"),
                              deps_mapper = function(x) extract_int(x, "(?<=H)\\d{2}")){
  list(
    deta_mapper = deta_mapper,
    deps_mapper = deps_mapper
  )
}
