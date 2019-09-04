test_that("calculation of the RUV variability works for a linearized model", {
  deps <- matrix(c(1,1,1,1, 0.1, 0.2, 0.3, 0.4), 4, 2)
  sigma <- diag(c(1, 0.05), 2, 2)
  omega <- diag(1, 1)
  depsdeta <- matrix(0, 4, 2)

  expect_equal(var_ruv_lf(deps, depsdeta, omega, sigma), diag(diag(deps %*% sigma %*% t(deps)), 4, 4)) # assuming no interaction

})
