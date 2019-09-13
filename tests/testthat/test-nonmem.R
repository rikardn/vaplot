test_that("column spec returns a list of quosures", {
  specs <- nm_column_specs()
  classes <- purrr::map(specs, class) %>% purrr::map_chr(1)
  expect_equal(classes, c(id = "quosure",
                          deta = "quosure", deps = "quosure", deps_deta = "quosure"))
})


test_that("default column mappers work", {
  mappers <- nm_column_mappers()
  expect_equal(mappers$deta_mapper(c(c("G101", "G021"))), c(10,2))
  expect_equal(mappers$deps_mapper(c(c("H031", "H121"))), c(3,12))
})
