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

test_that("lith_prep()", {
  f <- system.file("extdata", "test_gwells_lithology.csv", package = "bcaquiferdata")
  expect_silent(l <- lith_prep(f))
  expect_s3_class(l, "data.frame")
  expect_true(all(c("lithology_from_m", "lithology_to_m", "lithology_raw_combined") %in%
                    names(l)))
  expect_equal(l$lithology_raw_combined,
               paste(l$lithology_raw_data,
                      l$lithology_description_code,
                      l$lithology_material_code) |>
                 stringr::str_squish())
})

test_that("lith_prep() flags", {
  f <- system.file("extdata", "test_flags_lithology.csv", package = "bcaquiferdata")
  expect_silent(l <- lith_prep(f))

  expect_true(all(dplyr::select(l, dplyr::contains("flag"))))


})
