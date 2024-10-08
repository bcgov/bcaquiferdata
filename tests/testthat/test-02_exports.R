# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

test_that("wells_export() Strater", {

  # Preview data
  expect_silent(p <- wells_export(wells_eg_fixed, id = "mill", type = "strater",
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
    wells_export(wells_eg_fixed, id = "mill", type = "strater", dir = test_path()),
    "Writing Strater files")
  expect_equal(list.files(test_path(), "strater"),
               c("mill_strater_collars.csv", "mill_strater_lith.csv", "mill_strater_wls.csv"))

  expect_snapshot_value(p, style = "json2")

  unlink(list.files(test_path(), "^mill_strater", full.names = TRUE))
})


test_that("wells_export() Voxler", {

    # Preview data
    expect_silent(p <- wells_export(wells_eg_fixed, id = "mill", type = "voxler",
                                    preview = TRUE))
    expect_named(p, "voxler")

    expect_s3_class(p[["voxler"]], "data.frame")

    expect_named(
      p[["voxler"]],
      c("well_tag_number", "Easting_Albers", "Northing_Albers", "Water_Elevation",
        "Component"))

    # Save data
    expect_message(
      wells_export(wells_eg_fixed, id = "mill", type = "voxler", dir = test_path()),
      "Writing Voxler file")
    expect_equal(list.files(test_path(), "voxler"), "mill_voxler.csv")

    expect_snapshot_value(p, style = "json2")

    unlink(list.files(test_path(), "^mill_voxler", full.names = TRUE))

})

test_that("wells_export() ArcHydro", {

  # Preview data
  expect_silent(p <- wells_export(wells_eg_fixed, id = "mill", type = "archydro",
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
    wells_export(wells_eg_fixed, id = "mill", type = "archydro", dir = test_path()),
    "Writing ArcHydro files")
  expect_equal(list.files(test_path(), "archydro"),
               c("mill_archydro_bh.csv", "mill_archydro_hguid.csv", "mill_archydro_well.csv"))

  expect_snapshot_value(p, style = "json2")

  unlink(list.files(test_path(), "^mill_archydro", full.names = TRUE))
})

test_that("wells_export() Leapfrog", {

  # Preview data
  expect_silent(p <- wells_export(wells_eg_fixed, id = "mill", type = "leapfrog",
                                  preview = TRUE))
  expect_named(p, c("leapfrog_collars", "leapfrog_intervals"))

  expect_s3_class(p[["leapfrog_collars"]], "data.frame")
  expect_s3_class(p[["leapfrog_intervals"]], "data.frame")

  expect_named(
    p[["leapfrog_collars"]],
    c("Hole ID", "East (X)", "North (Y)", "Elev (Z)", "Max Depth"))

  expect_named(
    p[["leapfrog_intervals"]],
    c("Hole ID", "From", "To", "Lithology", "Lithology Raw"))

  # Save data
  expect_message(
    wells_export(wells_eg_fixed, id = "mill", type = "leapfrog", dir = test_path()),
    "Writing Leapfrog files")
  expect_equal(list.files(test_path(), "leapfrog"),
               c("mill_leapfrog_collars.csv", "mill_leapfrog_intervals.csv"))

  # Force fix if not fixed
  expect_message(p2 <- wells_export(wells_eg_unfixed, id = "mill", type = "leapfrog",
                              preview = TRUE),
                 "Fixing wells with a bottom lithology")
  expect_equal(p, p2)

  expect_snapshot_value(p, style = "json2")

  unlink(list.files(test_path(), "^mill_leapfrog", full.names = TRUE))
})

test_that("wells_export() Surfer", {

  # Preview data
  expect_silent(p <- wells_export(wells_eg_fixed, id = "mill", type = "surfer",
                                  preview = TRUE))
  expect_named(p, "surfer")

  expect_s3_class(p[["surfer"]], "data.frame")

  expect_named(
    p[["surfer"]],
    c("well_tag_number", "X", "Y", "bedrock_depth_m", "water_depth_m"))

  # Save data
  expect_message(
    wells_export(wells_eg_fixed, id = "mill", type = "surfer", dir = test_path()),
    "Writing Surfer file")
  expect_equal(list.files(test_path(), "surfer"), "mill_surfer.csv")

  expect_snapshot_value(p, style = "json2")

  unlink(list.files(test_path(), "^mill_surfer", full.names = TRUE))

})
