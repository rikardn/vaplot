nm_column_specs <- function(id = ID, time = TIME, eta = matches("^ETA?\\d+$"), deta = matches("^G\\d+$")){
  list(
    id = rlang::enquo(id),
    time = rlang::enquo(time),
    eta = rlang::enquo(eta),
    deta = rlang::enquo(deta)
  )
}
