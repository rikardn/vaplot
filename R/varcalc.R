# calculates the variability from the RUV model for linearized model
var_ruv_lf <- function(deps, depsdeta, omega, sigma){
  interaction_terms <- diag(diag(depsdeta %*% kronecker(omega, sigma) %*% t(depsdeta)), nrow = NROW(deps))
  diag(diag(deps %*% sigma %*% t(deps)),  nrow = NROW(deps)) + interaction_terms
}
