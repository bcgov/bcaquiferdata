test_that("wells_export()", {

  # Preview data for Strater
  p <- wells_export(mill_elev, id = "mill", type = "strater", preview = TRUE)
  names(p)
  p[["strater_lith"]]
  p[["strater_collars"]]
  p[["strater_wells"]]

  # Export data for Strater
  wells_export(creek_elev, id = "clinton", type = "strater")

  # Export Arc Hydro
  wells_export(creek_elev, id = "clinton", type = "archydro")



})
