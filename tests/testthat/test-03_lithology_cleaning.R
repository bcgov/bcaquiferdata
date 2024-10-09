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

test_that("lith_categorize()", {
  # Compare expected and produced results (stored in lith_expect())
  for(i in seq_len(length(lith_expect()))) {
    w <- lith_expect()[[i]]
    expect_equal(lith_categorize(w[[1]], w[[2]], w[[3]]), w[[4]], label = i)
  }

})


# To add tests, modify data-raw/test_data.R
# file.edit("data-raw/test_data.R")
test_that("lith_fix()", {

  t <- system.file("extdata", "test_lithology_cleaning.csv",
                   package = "bcaquiferdata") %>%
    readr::read_csv(col_types = "cc") %>%
    suppressWarnings()

  expect_silent(l <- lith_fix(t$desc[1])) |>
    expect_s3_class("data.frame")

  for(i in seq(nrow(t))) {
    expect_equal(lith_fix(desc = !!t$desc[i])[["lithology_category"]], !!t$cat[i])
  }

  # Check that removes erroneous "Aquifer Data:" prefaces
  expect_equal(lith_fix(desc = "aquifer data: glacial till")$lithology_extra,
               "")

  # Flags are consistent
  f <- stringr::str_subset(flags$Flag, "flag_cat_")
  expect_true(all(f %in% names(l)))
  expect_equal(sort(stringr::str_subset(names(l), "^flag_|^fix_")), sort(f))

})



test_that("lith_yield()", {
  t <- dplyr::tribble(
    ~lithology_raw_combined,   ~yield_units, ~flag_yield_digits, ~flag_yield_mismatch, ~yield, ~depth, ~depth_units,
    "2 to 3 gpm",     "gpm",  "", FALSE, 2.5,    as.double(), "",
    "50 gpm. and it takes 14 hrs. to recover", "gpm", "14", FALSE, 50, as.double(), "",
    "365' - 2 1/2 gpm  s", "gpm", "", FALSE, 2.5,   365, "ft",
    "45feet - 1/2-3 gpm","gpm", "", FALSE, 1.75, 45, "ft",
    "5 gpm at 50', 10 gpm at 75'", "gpm", "", FALSE, c(5, 10), c(50, 75), "ft",
    "60 and 80; 5gpm", "gpm", "60;80", FALSE, 5, as.double(), "",
    "425 feet 1 3/4 gpm", "gpm", "", FALSE, 1.75, 425, "ft",
    "fine t0 medium brown sand and gravel wet 3 gpm", "gpm", "0", FALSE, 3, as.double(), "",
    "1 1/2 gpm at 108'-120'", "gpm", "", TRUE, NA, NA, "ft",
    "1/2 gpm at 99' and 1 gpm at", "gpm", "", TRUE, NA, NA, "ft",
    ".5 gpm at 50'", "gpm", "",  FALSE, 0.5, 50, "ft",

    # Depth units
    "0.6 gpm at 100ft", "gpm", "", FALSE, 0.6, 100, "ft",
    "0.6 gpm at 100feet", "gpm", "", FALSE, 0.6, 100, "ft",
    "0.6 gpm at 100 feet", "gpm", "", FALSE, 0.6, 100, "ft",
    "0.6 gpm at 100 ft", "gpm", "", FALSE, 0.6, 100, "ft",
    "0.6 gpm at 100'", "gpm", "", FALSE, 0.6, 100, "ft",
    "0.6 gpm at 100m", "gpm", "", FALSE, 0.6, 100, "m",
    "0.6 gpm at 100metres", "gpm", "", FALSE, 0.6, 100, "m",
    "0.6 gpm at 100meter", "gpm", "", FALSE, 0.6, 100, "m",
    "0.6 gpm at 100 m", "gpm", "", FALSE, 0.6, 100, "m",
  ) %>%
    dplyr::select("lithology_raw_combined", "flag_yield_digits", "flag_yield_mismatch",
                  "depth", "depth_units", "yield", "yield_units")

  l <- lith_yield(dplyr::select(t, "lithology_raw_combined", "yield_units"))

  expect_equal(l, tidyr::unnest(t, c("yield", "depth"), keep_empty = TRUE))

})



