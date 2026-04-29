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

# Clean up
unlink(list.files(tempdir()))

test_that("wells_export() Strater - preview", {
  # Preview data
  expect_silent(
    p <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "strater",
      preview = TRUE
    )
  )
  expect_named(p, c("strater_lith", "strater_collars", "strater_wls"))

  expect_s3_class(p[["strater_lith"]], "data.frame")
  expect_s3_class(p[["strater_collars"]], "data.frame")
  expect_s3_class(p[["strater_wls"]], "data.frame")

  expect_named(
    p[["strater_lith"]],
    c("Hole_ID", "From", "To", "Lithology_Keyword", "Lithology_Description")
  )

  expect_named(
    p[["strater_collars"]],
    c(
      "Hole_ID",
      "Easting_Albers",
      "Northing_Albers",
      "Starting_Depth",
      "Ending_Depth",
      "Elevation"
    )
  )

  expect_named(p[["strater_wls"]], c("well_tag_number", "water_depth_m"))
  expect_snapshot_value(p, style = "json2")
})

test_that("wells_export() Strater - save", {
  # Save data
  expect_message(
    wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "strater",
      dir = test_path()
    ),
    "Writing Strater file\\(s\\)"
  )
  expect_equal(
    list.files(test_path(), "strater"),
    c(
      "mill_strater_collars.csv",
      "mill_strater_lith.csv",
      "mill_strater_wls.csv"
    )
  )

  unlink(list.files(test_path(), "^mill_strater", full.names = TRUE))
})

test_that("wells_export() Strater - zip", {
  # Save data
  expect_message(
    f <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "strater",
      dir = test_path(),
      zip = TRUE
    ),
    "Writing Strater file\\(s\\)"
  ) |>
    expect_message("Zipping files...") |>
    suppressMessages()

  expect_true(file.exists(f))
  expect_equal(list.files(test_path(), "strater"), "mill_strater.zip")

  unlink(list.files(test_path(), "^mill_strater", full.names = TRUE))
})


test_that("wells_export() Voxler - preview", {
  # Preview data
  expect_silent(
    p <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "voxler",
      preview = TRUE
    )
  )
  expect_named(p, "voxler")

  expect_s3_class(p[["voxler"]], "data.frame")

  expect_named(
    p[["voxler"]],
    c(
      "well_tag_number",
      "Easting_Albers",
      "Northing_Albers",
      "Water_Elevation",
      "Component"
    )
  )
  expect_snapshot_value(p, style = "json2")
})

test_that("wells_export() Voxler - save", {
  # Save data
  expect_message(
    wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "voxler",
      dir = test_path()
    ),
    "Writing Voxler file"
  )
  expect_equal(list.files(test_path(), "voxler"), "mill_voxler.csv")

  unlink(list.files(test_path(), "^mill_voxler", full.names = TRUE))
})

test_that("wells_export() Voxler - zip", {
  # Save data
  expect_message(
    f <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "voxler",
      dir = test_path(),
      zip = TRUE
    ),
    "Writing Voxler file\\(s\\)"
  ) |>
    expect_message("Skipping zip for single Voxler file")

  expect_true(file.exists(f))
  expect_equal(list.files(test_path(), "voxler"), "mill_voxler.csv")

  unlink(list.files(test_path(), "^mill_voxler", full.names = TRUE))
})

test_that("wells_export() ArcHydro - preview", {
  # Preview data
  expect_silent(
    p <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "archydro",
      preview = TRUE
    )
  )
  expect_named(p, c("archydro_well", "archydro_hguid", "archydro_bh"))

  expect_s3_class(p[["archydro_well"]], "data.frame")
  expect_s3_class(p[["archydro_hguid"]], "data.frame")
  expect_s3_class(p[["archydro_bh"]], "data.frame")

  expect_named(
    p[["archydro_well"]],
    c("HydroID", "HydroCode", "X", "Y", "LandElev", "WellDepth")
  )

  expect_named(
    p[["archydro_hguid"]],
    c("HGUID", "HGUCode", "Description", "HGUName")
  )

  expect_named(
    p[["archydro_bh"]],
    c(
      "WellID",
      "WellCode",
      "Material",
      "HGUID",
      "RefElev",
      "FromDepth",
      "ToDepth",
      "TopElev",
      "BottomElev",
      "OriginalLithology"
    )
  )
  expect_snapshot_value(p, style = "json2")
})


test_that("wells_export() ArcHydro - save", {
  # Save data
  expect_message(
    wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "archydro",
      dir = test_path()
    ),
    "Writing ArcHydro file\\(s\\)"
  )
  expect_equal(
    list.files(test_path(), "archydro"),
    c(
      "mill_archydro_bh.csv",
      "mill_archydro_hguid.csv",
      "mill_archydro_well.csv"
    )
  )

  unlink(list.files(test_path(), "^mill_archydro", full.names = TRUE))
})


test_that("wells_export() ArcHydro - zip", {
  # Save data
  expect_message(
    f <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "archydro",
      dir = test_path(),
      zip = TRUE
    ),
    "Writing ArcHydro file\\(s\\)"
  ) |>
    expect_message("Zipping files...") |>
    suppressMessages()

  expect_true(file.exists(f))
  expect_equal(list.files(test_path(), "archydro"), "mill_archydro.zip")

  unlink(list.files(test_path(), "^mill_archydro", full.names = TRUE))
})


test_that("wells_export() Leapfrog - preview", {
  # Preview data
  expect_message(
    p <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "leapfrog",
      preview = TRUE
    ),
    "Fixing wells where depth"
  )
  expect_named(p, c("leapfrog_collars", "leapfrog_intervals"))

  expect_s3_class(p[["leapfrog_collars"]], "data.frame")
  expect_s3_class(p[["leapfrog_intervals"]], "data.frame")

  expect_named(
    p[["leapfrog_collars"]],
    c(
      "Aquifer ID",
      "Hole ID",
      "East (X)",
      "North (Y)",
      "Elev (Z)",
      "Max Depth (m)",
      "Artesian Conditions",
      "Artesian Pressure (Head Ft AGL)"
    )
  )

  expect_named(
    p[["leapfrog_intervals"]],
    c("Hole ID", "From", "To", "Lithology", "Lithology Raw")
  )

  # Force fix if not fixed
  expect_message(
    p2 <- wells_export(
      wells_eg_unfixed,
      id = "mill",
      type = "leapfrog",
      preview = TRUE
    ),
    "Fixing wells with a bottom lithology"
  ) %>%
    expect_message("Fixing wells missing depth") %>%
    expect_message("Fixing wells where depth")
  expect_equal(p, p2)

  expect_snapshot_value(p, style = "json2")
})

test_that("wells_export() Leapfrog - save", {
  # Save data
  expect_message(
    wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "leapfrog",
      dir = test_path()
    ),
    "Writing Leapfrog file\\(s\\)"
  ) %>%
    suppressMessages()
  expect_equal(
    list.files(test_path(), "leapfrog"),
    c("mill_leapfrog_collars.csv", "mill_leapfrog_intervals.csv")
  )

  unlink(list.files(test_path(), "^mill_leapfrog", full.names = TRUE))
})


test_that("wells_export() Leapfrog - zip", {
  # Save data
  expect_message(
    f <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "leapfrog",
      dir = test_path(),
      zip = TRUE
    ),
    "Writing Leapfrog file\\(s\\)"
  ) |>
    expect_message("Zipping files...") |>
    suppressMessages()

  expect_true(file.exists(f))
  expect_equal(list.files(test_path(), "leapfrog"), "mill_leapfrog.zip")

  unlink(list.files(test_path(), "^mill_leapfrog", full.names = TRUE))
})

test_that("wells_export() Leapfrog duplicates Koksilha", {
  skip_if(
    !file.exists(
      m <- test_path(
        "../../misc/data/Koksilah_watershed4/Koksilah_watershed4.shp"
      )
    )
  )

  r <- sf::st_read(m, quiet = TRUE)
  expect_message(d_l <- dem_region(r, source = "lidar")) |>
    suppressMessages()
  expect_message(d_t <- dem_region(r, source = "trim")) |>
    suppressMessages()
  expect_message(w <- wells_subset(r)) |>
    suppressMessages()
  expect_warning(
    e <- wells_elev(w, dem = d_l, dem_extra = d_t),
    "Combining elevations measured through different techniques"
  ) |>
    suppressMessages()

  # Preview
  expect_message(
    p <- wells_export(
      e,
      id = "koksilah",
      type = "leapfrog",
      preview = TRUE
    )
  )
})

test_that("wells_export() Surfer - preview", {
  # Preview data
  expect_silent(
    p <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "surfer",
      preview = TRUE
    )
  )
  expect_named(p, "surfer")

  expect_s3_class(p[["surfer"]], "data.frame")

  expect_named(
    p[["surfer"]],
    c("well_tag_number", "X", "Y", "bedrock_depth_m", "water_depth_m")
  )
  expect_snapshot_value(p, style = "json2")
})

test_that("wells_export() Surfer - save", {
  # Save data
  expect_message(
    wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "surfer",
      dir = test_path()
    ),
    "Writing Surfer file\\(s\\)"
  )
  expect_equal(list.files(test_path(), "surfer"), "mill_surfer.csv")

  unlink(list.files(test_path(), "^mill_surfer", full.names = TRUE))
})


test_that("wells_export() Surfer - zip", {
  # Save data
  expect_message(
    f <- wells_export(
      wells_eg_fixed,
      id = "mill",
      type = "surfer",
      dir = test_path(),
      zip = TRUE
    ),
    "Writing Surfer file\\(s\\)"
  ) |>
    expect_message("Skipping zip for single Surfer file")

  expect_true(file.exists(f))
  expect_equal(list.files(test_path(), "surfer"), "mill_surfer.csv")

  unlink(list.files(test_path(), "^mill_surfer", full.names = TRUE))
})
