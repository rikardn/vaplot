test_that("Schur complement calculation works", {
  m <- diag(1:10)
  expect_equal(schur_complement(m, 2), m[-2,-2])
})

test_that("linearized variance for f is calculated correctly", {
  df <- c(1,1)
  omega <- matrix(c(2,1,1,2),2,2)
  expect_equal(lvar_f(df, omega), matrix(2+2+2*1,1,1))
})
