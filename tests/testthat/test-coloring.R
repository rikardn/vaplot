test_that("color function returns a vector of the right length", {
  res <- list(column_types = c(TIME = "idv", ETA1 = "variability", ETA2 = "variability", RUV = "variability"))
  expect_named(color_like_hadley(res) , c("ETA1", "ETA2", "RUV"))
})
