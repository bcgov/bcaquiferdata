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

test_that("lith_flags_interval() flags", {

  # No problems
  expect_message(l <- lith_flags_interval(test_lith_flags("none")))
  expect_true(all(!dplyr::select(l, dplyr::contains("flag"))))

  # Overruns
  expect_message(l <- lith_flags_interval(test_lith_flags("overruns")))
  expect_true(all(l$flag_int_overrun[2:3])) # Get overruns
  expect_true(all(!l$flag_int_overlap))     # Do not get overlaps (already marked)

  # Overlaps
  expect_message(l <- lith_flags_interval(test_lith_flags("overlaps")))
  expect_true(all(l$flag_int_overlap[1:2]))

  # Gaps
  expect_message(l <- lith_flags_interval(test_lith_flags("gaps")))
  expect_true(all(l$flag_int_gap[1:2]))

  # Shortform
  expect_message(l <- lith_flags_interval(test_lith_flags("shortform")))
  expect_true(all(l$flag_int_shortform[2:3]))

  # Bottom
  expect_message(l <- lith_flags_interval(test_lith_flags("bottom")))
  expect_true(all(l$flag_int_bottom[3]))

  # Missing
  expect_message(l <- lith_flags_interval(test_lith_flags("missing")))
  expect_true(all(l$flag_int_missing[2]))

  # More complex
  expect_message(l <- lith_flags_interval(test_lith_flags("complex")))
  expect_true(all(l$flag_int_note[1]))
  expect_true(all(l$flag_int_shortform[3:4]))

  # Flags are consistent
  f <- stringr::str_subset(flags$Flag, "flag_int_")
  expect_true(all(f %in% names(l)))
  expect_equal(sort(stringr::str_subset(names(l), "^flag_|^fix_")), sort(f))
})

test_that("lith_prep()", {
  f <- system.file("extdata", "test_gwells_lithology.csv", package = "bcaquiferdata")
  expect_message(l <- lith_prep(f)) |>
    suppressMessages()
  expect_s3_class(l, "data.frame")
  expect_true(all(c("lithology_from_m", "lithology_to_m", "lithology_raw_combined") %in%
                    names(l)))
  expect_equal(l$lithology_raw_combined,
               paste(l$lithology_raw_data,
                      l$lithology_description_code,
                      l$lithology_material_code) |>
                 stringr::str_squish())
})

