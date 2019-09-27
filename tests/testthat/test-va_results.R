
test_that("va_results can be created", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10, `ETA(1)` = 1),
                    results_col("TIME", "idv"),
                    results_col("ETA(1)","variability", "ETA1", "iiv-var")
                    )
  expect_is(res, "va_results")
  expect_named(res, c("table", "column_specs"))
})


test_that("va_results can be printed to the console", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10, `ETA(1)` = 1),
                    results_col("TIME", "idv"),
                    results_col("ETA(1)","variability", list("ETA1"), list("iiv-var"))
  )
  expect_output(print(res))
})

test_that("different columns can be retrieved", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10, `ETA(1)` = 1),
                    results_col("TIME", "idv"),
                    results_col("ETA(1)","variability", list("ETA1"), list("iiv-var")),
                    results_col("ETA(2)","variability", list("ETA2"), list("iiv-var")),
                    results_col("WT","variability", list("WT"), list("covariate")),
                    results_col("DVID","facet-var"))
  expect_equal(get_variability_cols(res), c("ETA(1)", "ETA(2)", "WT"))
  expect_equal(get_idv_col(res), "TIME")
  expect_equal(get_facet_cols(res), "DVID")
  expect_equal(get_cov_dependent_cols(res), "WT")
})
