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




test_that("dsi_sources can be concatenated from other dsi_sources", {
  x <- iris %>% set_dsinfo(sources = dsi_source("foo", "foo@corp.at"))
  y <- iris %>% set_dsinfo(sources = dsi_source("bar", "bar@corp.at"))

  res <- sources(x, y, iris, dsi_source("hash", "baz@corp.at"))
  expect_setequal(
    vapply(res, `[[`, character(1), "title"),
    c("foo", "bar", "hash")
  )


  r2 <- set_dsinfo(res, sources = res)


  expect_identical(res, sources(r2))
  expect_identical(res, sources(dsinfo(r2)))
})




test_that("dsi_sources can be consolidated from other dsi_sources", {
  x <- iris %>% set_dsinfo(sources = dsi_source("foo", email = "foo@corp.at", date = as.Date("2019-12-01")))
  y <- iris %>% set_dsinfo(sources = dsi_source("foo", email = "foo@corp.at", date = as.Date("2019-12-01")))
  z <- iris %>% set_dsinfo(sources = dsi_source("foo", email = "foo@corp.at", date = as.Date("2019-12-02")))

  dd <- sources(x, y, z, iris, dsi_source("hash", "baz@corp.at"))

  res <- consolidate_sources(dd)

  res

})




test_that("accessing and setting sources with sources() works", {
  x <- iris
  sources(x) <- dsi_source("test1")
  expect_identical(sources(x)[[1]]$title, "test1")
  sources(x) <- dsi_source("test2")
  expect_identical(sources(x)[[1]]$title, "test2")
})
