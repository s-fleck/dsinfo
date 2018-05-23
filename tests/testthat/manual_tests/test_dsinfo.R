test_that("dsinfo sources print nicely", {
  #* @testing dsi_sources
  #* @testing format_source

  x <- set_dsinfo(
    iris,
    id = "iris001",
    reference_date = Sys.Date(),
    name = "iris_dataset",
    title = "The Iris Dataset",
    version = "1.0",
    sources = dsi_sources(
      dsi_source("R demo data", path = "path/to/data", date = Sys.Date()),
      dsi_source("Alfred Bogus", email = c("alfred@bogus.xx", "alfred.bogus@yahoo.ru"))
    ),
    custom_metadata = "blubb",
    description = paste(rep("blah", 200), collapse = " "),
    homepage = "http://www.blah.bl"
  )
  dsinfo(x)


  x <- set_dsinfo(
    iris,
    id = "iris001",
    name = "iris_dataset",
    title = "The Iris Dataset",
    sources = dsi_sources(
      dsi_source("R demo data", path = "path/to/data", date = Sys.Date()),
      dsi_source("Alfred Bogus", email = c("alfred@bogus.xx", "alfred.bogus@yahoo.ru"))
    )
  )
  dsinfo(x)


  x <- set_dsinfo(
    iris,
    id = "iris001",
    name = "iris_dataset",
    title = "The Iris Dataset"
  )

  dsinfo(x)

})
