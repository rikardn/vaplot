skip_if_not_installed("xpose")

xpdb <- xpose::xpose_data(file=system.file("testdata", "warfarin-pkpd.lst", package = "vaplot"))

test_that("reading derivative results works",{
  res <- read_nm_derivative_results(system.file("testdata", "derivatives_run4.lst", package = "vaplot"))
  expect_named(res, c("thetavec", "iivmat", "ruvmat", "derivdata"), ignore.order = TRUE)
})

test_that("fetching table data works", {
  tab <- get_table_data(xpdb)
  expect_is(tab, "data.frame")
})


test_that("fetching theta estimates works", {
  expected <- c(THETA1 = 0.137876, THETA2 = 8.13688, THETA3 = 0.706843,
                THETA4 = 96.8055,  THETA5 = -75.2148, THETA6 = 0.249284)
  expect_equal(get_theta_vector(xpdb, 1), expected)
})

test_that("fetching omega estimates works", {
  expected <- structure(c(0.0743618, 0, 0, 0, 0, 0,
                          0, 0.103073, 0, 0, 0, 0,
                          0, 0, 0.777453, 0, 0, 0,
                          0, 0, 0, 0.00136738, 0, 0,
                          0, 0, 0,  0, 0.0080745, 0,
                          0, 0, 0, 0, 0, 0.521603), .Dim = c(6L, 6L))
  expect_equal(get_omega_matrix(xpdb, 1), expected)
})

test_that("fetching sigma estimates works",{
  expected <- structure(c(0.00731498, 0, 0, 0, 0.062858, 0, 0, 0, 28.9374), .Dim = c(3L,  3L))
  expect_equal(get_sigma_matrix(xpdb, 1), expected)
})
