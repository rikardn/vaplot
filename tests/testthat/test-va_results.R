
test_that("va_results can be created", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10),
                    column_types = c(idv = "TIME"))
  expect_is(res, "va_results")
  expect_named(res, c("table", "column_types"))
})


test_that("va_results can be printed to the console", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10),
                    column_types = c(idv = "TIME"))
  expect_output(print(res))
})
