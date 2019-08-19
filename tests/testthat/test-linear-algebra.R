test_that("Schur complement calculation works", {
  m <- diag(1:10)
  expect_equal(schur_complement(m, 2), m[-2,-2])
})
