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

lith_expect <- function(){
  # primary (X), secondary (with X), tertiary (Xy, or Xly), Category
  list(
    list(c("sand", "gravel"), "",      "",      "Sand and Gravel (Clean)"),
    list("sand",              "",      "",      "Sand"),
    list("clay",              "",      "",      "Clay"),
    list("gravel",            "",      "",      "Gravel"),
    list("till",              "sand",  "",      "Sand or Gravel Till or Diamicton"),
    list("till",              "",      "sand",  "Sand or Gravel Till or Diamicton"),
    list("bedrock",           "bedrock", "",    "Bedrock")
  )
}

test_lith_flags <- function(type = "all") {
  flg <- dplyr::tribble(
    ~well_tag_number, ~lithology_from_m, ~lithology_to_m, ~lithology_raw_combined,
    # No problems - 1:3
    1, 0, 10, "clay",
    1, 10, 20, "gravel",
    1, 20, 30, "sand",
    # Overruns - 4:7
    2, 0, 10, "clay",
    2, 0, 0, "clay",
    2, 0, 0, "clay",
    2, 10, 20, "clay",
    # Overlaps - 8:10
    3, 0, 10, "clay",
    3, 5, 15, "clay",
    3, 15, 20, "clay",
    # Gaps - 11:13
    4, 0, 10, "clay",
    4, 12, 20, "clay",
    4, 20, 30, "clay",
    # Short form - 14:16
    5, 0, 10, "clay",
    5, 0, 20, "clay",
    5, 0, 30, "clay",
    # Bottom - 17:19
    6, 0, 10, "clay",
    6, 10, 20, "clay",
    6, 20, 20, "bedrock",
    # Missing - 20:22
    7, 0, 10, "clay",
    7, 10, 20, "",
    7, 20, 30, "clay",
    # Short form complex - 23:26
    8, 0, 0, "General notes",
    8, 0, 10, "first layer",
    8, 0, 20, "secondc layer",
    8, 0, 30, "third layer"
  ) %>%
    dplyr::mutate(n = dplyr::n(), rec_no = dplyr::row_number(), .by = "well_tag_number")

  n <- switch(
    type,
    "all" = 1:22,
    "none" = 1:3,
    "overruns" = 4:7,
    "overlaps" = 8:10,
    "gaps" = 11:13,
    "shortform" = 14:16,
    "bottom" = 17:19,
    "missing" = 20:22,
    "complex" = 23:26
  )

  flg[n,]
}
