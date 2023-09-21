test_that("lidar_fetch() fails without tiles", {
  nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
  sf::st_crs(nc) <- 4326
  expect_error(lidar_fetch(nc, quiet = TRUE), "Region does not overlap any BC tiles")
})
