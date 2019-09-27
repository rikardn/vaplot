test_that("color function returns a vector of the right length", {
  res <- va_results(table = dplyr::tibble(TIME = 1:10, `ETA(1)` = 1),
                    results_col("TIME", "idv"),
                    results_col("ETA(1)","variability", list("ETA1"), list("iiv-var")),
                    results_col("ETA(2)","variability", list("ETA2"), list("iiv-var")),
                    results_col("WT","variability", list("WT"), list("covariate")),
                    results_col("DVID","facet-var"))
  expect_named(color_like_hadley(res) , c("ETA(1)", "ETA(2)", "WT"))
})
