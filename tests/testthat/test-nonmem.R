test_that("column spec returns a list of quosures", {
  specs <- nm_column_specs()
  classes <- purrr::map(specs, class) %>% purrr::map_chr(1)
  expect_equal(classes, c(id = "quosure", time = "quosure", eta = "quosure", deta = "quosure"))
})
