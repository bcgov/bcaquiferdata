test_that("lidar_fetch() fails without tiles", {
  skip_if_not(file.exists(f <- system.file("shapes/sids.shp", package="spData")[1]))
  nc <- sf::st_read(f, quiet = TRUE)
  sf::st_crs(nc) <- 4326
  expect_error(lidar_fetch(nc, quiet = TRUE), "Region does not overlap any BC tiles")
})
