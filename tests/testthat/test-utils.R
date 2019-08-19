test_that("vector to matrix conversion works", {
  expect_equal(lower_tri_vec_to_mat(c(1,2,1)), matrix(c(1,2,2,1),2,2))
})
