context("dsi_source")


test_that("dsi_sources can be concatenated from other dsi_sources", {
  tf <- tempfile()
  x <- dsi_source("blubb", "blah@blubb.at")
  y <- dsi_sources_from_paths(c(tf, tempfile(), tempfile()))

  res <- dsi_sources(x, y)

  expect_identical(length(res), 4L)
  expect_true(all(
    vapply(res, is_dsinfo_source, F)
  ))

})
