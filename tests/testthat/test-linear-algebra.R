test_that("Schur complement calculation works", {
  m <- diag(1:10)
  expect_equal(schur_complement(m, 2), m[-2,-2])
})

test_that("Schur complement calculation for named matricies works", {
  m <- set_rcnames(diag(1:10), paste0("eta", 1:10))
  expect_equal(schur_complement_named(m, "eta2"), m[-2,-2])
})


test_that("variance for linearized f is calculated correctly", {
  df <- c(1,1)
  omega <- matrix(c(2,1,1,2),2,2)
  expect_equal(var_lf(df, omega), matrix(2+2+2*1,1,1))
})


test_that("conditional variance for linearized f is calculated correctly", {
  df <- c(a = 1, b = 1)
  omega <- matrix(c(2,0,0,1),2,2, dimnames = list(c("a","b"), c("a","b")))
  expect_equal(condvar_lf(df, omega, "a"), matrix(1,1,1))
})

test_that("ginv behaves like normal inv for invertible matricies",{
  x <- matrix(c(1,2,2,1), 2,2, dimnames = list(c("A","B"), c("A","B")))
  expect_equal(solve(x), ginv(x))
})
