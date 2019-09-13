#'@export
prepare_va_fun <- function(model_fun, theta, omega, sigma, time, z){

  if(!requireNamespace("numDeriv", quietly = TRUE))
    ui_error("The 'numDeriv' package could not be loaded but is required for models specified as function",
             suggestions = c("Install 'numDeriv' using: install.packages('numDeriv')"))

  neta <- NCOL(omega)
  neps <- NCOL(sigma)


  eta_samples <- MASS::mvrnorm(100, mu = rep(0, neta), Sigma = omega)
  colnames(eta_samples) <- paste0("ETA",1:neta)
  colnames(omega) <- rownames(omega) <- paste0("ETA",1:neta)
  #eps_samples <- MASS::mvrnorm(100, mu = rep(0, neps), Sigma = sigma)

  eps_zero <- rep(0, neps)
  derivative_data <- apply(eta_samples, 1,
                           function(eta){
                             purrr::update_list(calc_derivatives(model_fun,
                                              time = time,
                                              theta = theta,
                                              eta =eta,
                                              eps = eps_zero,
                                              z = z),
                                  eta = matrix(eta, nrow = 1, ncol = neta, dimnames = list(NULL, names(eta))),
                                  other = dplyr::tibble(TIME = time)
                               )
                            }
                           )

  inp <- va_input(
    column_names = "TIME",
    theta = theta,
    omega = omega,
    sigma = sigma,
    derivative_data = derivative_data,
    input_file = "model"
  )
}


calc_derivatives <- function(model_fun, time, theta, eta, eps, z){
  neta <- length(eta)
  neps <- length(eps)
  dinf <- numDeriv::genD(function(eps_eta) my_model(time, theta,
                                               eta = eps_eta[seq_len(neta)],
                                               eps = eps_eta[-seq_len(neta)],
                                               z = z), x = c(eta, eps))
  deta <- dinf$D[,seq_len(neta), drop=FALSE]
  colnames(deta) <- names(eta)
  deps <- dinf$D[,seq(neta+1, neta + neps, by = 1), drop=FALSE]
  deps_deta <- dinf$D[,eta_eps_indicies(neta, neps), drop=FALSE]
  return(
    list(deta = deta,
         deps = deps,
         deps_deta = deps_deta)
    )
}

# calculate array index from matrix index for lower triangular matrix
index_genD <- function(i, j, neta, neps) as.integer((i-1L)*(i)/2L + j + neta + neps)

# get all indicies for deps_deta derivatives
eta_eps_indicies <- function(neta, neps) {
  purrr::cross(list(i = seq.int(neta + 1,neta + neps,1), j = seq.int(1,neta,1))) %>%
  purrr::map_int(~rlang::exec(index_genD, !!!.x, neta = neta, neps = neps))
}

