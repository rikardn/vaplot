res <- va_results(table = dplyr::tibble(TIME = 1:10),
                  column_names = c(idv = "TIME"))

test_that("va_results can be created", {
  expect_is(res, "va_results")
  expect_named(res, c("table", "column_names"))
})


test_that("va_results can be printed to the console", {
  expect_output(print(res))
})
