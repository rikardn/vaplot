test_that("vector to matrix conversion works", {
  expect_equal(lower_tri_vec_to_mat(c(1)), matrix(c(1),1,1))
  expect_equal(lower_tri_vec_to_mat(c(1,2,1)), matrix(c(1,2,2,1),2,2))
})

test_that("dropping rows and columns in a matrix works", {
  m <- matrix(1:4, 2, 2, dimnames = list(c("a","b"), c("a", "b")))
  expect_equal(drc(m, "b"), m[1,1, drop = F])
})

test_that("selecting rows and columns in a matrix works", {
  m <- matrix(1:4, 2, 2, dimnames = list(c("a","b"), c("a", "b")))
  expect_equal(src(m, "b"), m[2,2, drop = F])
})

test_that("dropping rows and selecting columns in a matrix works", {
  m <- matrix(1:9, 3, 3, dimnames = list(c("a","b","c"), c("a", "b","c")))
  expect_equal(dsrc(m, select_cols = "c" , drop_rows = "b"), m[c(1,3), 3, drop = F])
})



test_that("dropping rows in a vector works", {
  vec <- c(a = 1, b = 2, c = 3)
  expect_equal(dr(vec, "b"), vec[-2])
})

test_that("selecting rows in a vector works", {
  vec <- c(a = 1, b = 2, c = 3)
  expect_equal(sr(vec, "b"), vec[2])
})

test_that("setting row and colnames for matricies works",{
  m <- diag(1, 2, 2)
  m <- set_rcnames(m, c("eta1", "eta2"))
  expect_equal(m, structure(c(1, 0, 0, 1), .Dim = c(2L, 2L), .Dimnames = list(c("eta1",  "eta2"), c("eta1", "eta2"))))
})

test_that("extracting indicies through regex works", {
  expect_equal(extract_int(c("G101", "G021"), "(?<=G)\\d{2}"), c(10,2))
})
