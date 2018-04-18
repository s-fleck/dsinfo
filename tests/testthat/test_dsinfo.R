context("dsinfo")


test_that("dsinfo works as expected", {

  x <- 1L

  expect_silent(
    x <- set_dsinfo(x,
      # data package recommended
      name = "test_data",
      id = "t001",
      version = "0.0.1",
      reference_date = as.Date("2016-01-01"),

      # optional
      source_date = as.Date("2016-01-01"),
      source_path = "/",

      # data package recommended
      license = "CC",

      # data package optional
      title = "A test dataset",
      description = "A dataset created for testing purposes \n* test \n* data",
      homepage = "www.zombo.com",

      contributors = "Foobert Bar",
      keywords = c("test", "data"),
      created = Sys.time(),


      # data package-compat
      profile = NULL,  #recommended
      image = NULL,  #optional
      blah = "blubb"
    )
  )

  expect_silent(set_dsinfo(x, reference_date = Sys.Date()))
  expect_silent(set_dsinfo(x, reference_date = lubridate::interval(Sys.Date(), Sys.Date())))  #nolint
  expect_silent(set_dsinfo(x, reference_date = dint::as_date_yq(Sys.Date())))
  expect_error(set_dsinfo(x, reference_date = "x"))


  y <- set_dsinfo(1L, homepage = "www.zombo.com")
})




test_that("dsinfo works as expected", {
  expect_true(is_dsinfo_name("ab.1"))
  expect_true(is_dsinfo_name("ab_1"))
  expect_true(is_dsinfo_name("ab-1"))
  expect_false(is_dsinfo_name("ab(1"))
})




test_that("reference_date works as expected", {
  x <- 1L

  expect_silent(
    reference_date(x) <- dint::as_date_ym(201612)
  )

  expect_identical(
    reference_date(x),
    date_ym(2016, 12)
  )

  expect_true(
    has_reference_date(x)
  )

  y <- 2L

  y <- set_reference_date(y, reference_date(x))

  expect_identical(
    reference_date(x),
    reference_date(y)
  )
})




test_that("test print method", {
  x <- set_dsinfo(
    iris,
    name = "iris",
    title = "iris data set",
    reference_date = Sys.Date(),
    sources = dsi_source(title = "wd", path = getwd() )
  )

  dsinfo(x)


  x <- set_dsinfo(
    iris,
    name = "iris",
    title = "iris data set",
    reference_date = Sys.Date(),
    sources = dsi_sources(
      dsi_source(title = "wd", path = getwd() ),
      dsi_source(title = "wd2", path = getwd() ))
  )


  dsinfo(x)
})




test_that("dsinfo .add = TRUE works as expected", {
  x <- set_dsinfo(
    iris,
    name = "iris",
    title = "iris data set",
    reference_date = Sys.Date(),
    sources = dsi_source(title = "wd", path = getwd() ),
    .add = TRUE
  )

  expect_identical(dsinfo(x)$title, "iris data set")
  expect_identical(dsinfo(x)$name,  "iris")

  x <- set_dsinfo(x, name = "iris2", .add = TRUE)

  expect_identical(dsinfo(x)$title, "iris data set")
  expect_identical(dsinfo(x)$name,  "iris2")
})
