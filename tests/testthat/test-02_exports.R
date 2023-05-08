test_that("wells_export() Strater", {

  # Preview data
  expect_silent(p <- wells_export(mill_elev, id = "mill", type = "strater",
                                  preview = TRUE))
  expect_named(p, c("strater_lith", "strater_collars", "strater_wells"))

  expect_s3_class(p[["strater_lith"]], "data.frame")
  expect_s3_class(p[["strater_collars"]], "data.frame")
  expect_s3_class(p[["strater_wells"]], "data.frame")

  expect_named(
    p[["strater_lith"]],
    c("Hole_ID", "From", "To", "Lithology_Keyword", "Lithology_Description"))

  expect_named(
    p[["strater_collars"]],
    c("Hole_ID", "Easting_Albers", "Northing_Albers", "Starting_Depth",
      "Ending_Depth", "Elevation"))

  expect_named(p[["strater_wells"]], c("well_tag_number", "water_depth_m"))


  # Save data
  expect_message(
    wells_export(mill_elev, id = "mill", type = "strater", dir = test_path()),
    "Writing Strater files")

  unlink(list.files(test_path(), "^mill_", full.names = TRUE))
})


test_that("wells_export() Voxler", {

    # Preview data
    expect_silent(p <- wells_export(mill_elev, id = "mill", type = "voxler",
                                    preview = TRUE))
    expect_named(p, "voxler")

    expect_s3_class(p[["voxler"]], "data.frame")

    expect_named(
      p[["voxler"]],
      c("well_tag_number", "Easting_Albers", "Northing_Albers", "Water_Elevation",
        "Component"))

    # Save data
    expect_message(
      wells_export(mill_elev, id = "mill", type = "voxler", dir = test_path()),
      "Writing Voxler file")

    unlink(list.files(test_path(), "^mill_vox", full.names = TRUE))

})

test_that("wells_export() ArcHydro", {

  # Preview data
  expect_silent(p <- wells_export(mill_elev, id = "mill", type = "archydro",
                                  preview = TRUE))
  expect_named(p, c("archydro_well", "archydro_hguid", "archydro_bh"))

  expect_s3_class(p[["archydro_well"]], "data.frame")
  expect_s3_class(p[["archydro_hguid"]], "data.frame")
  expect_s3_class(p[["archydro_bh"]], "data.frame")

  expect_named(
    p[["archydro_well"]],
    c("HydroID", "HydroCode", "X", "Y", "LandElev", "WellDepth"))

  expect_named(
    p[["archydro_hguid"]], c("HGUID", "HGUCode", "Description", "HGUName"))

  expect_named(
    p[["archydro_bh"]],
    c("WellID", "WellCode", "Material", "HGUID", "RefElev", "FromDepth",
      "ToDepth", "TopElev", "BottomElev", "OriginalLithology"))

  # Save data
  expect_message(
    wells_export(mill_elev, id = "mill", type = "archydro", dir = test_path()),
    "Writing ArcHydro files")

  unlink(list.files(test_path(), "^mill_arc", full.names = TRUE))
})
