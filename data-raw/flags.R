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

flags <- dplyr::tribble(

  ~ "Flag", ~ "Description",

  # Whole record flags
  "flag_missing", "Missing lithology record",
  "flag_no_depths", "All lithology records have depths of 0 ('from' and 'to')",
  "flag_overruns", paste0("Some lithology records have depths of 0 ('from' and 'to'). ",
                          "Possibly a second entry for a single record, 'overrun' record"),
  "flag_bottom_unit", "",
  "flag_zero_zero", "Any lithology record that has a depth of 0 to 0",

  # Specific observation flags
  "flag_bedrock", "Bedrock should be the only primary term in a lithology record",
  "flag_boulders", "Boulders should be the only primary term in a lithology record",
  "flag_missing_cats", "No categories were extracted from the cleaned lithology record"
)

usethis::use_data(flags, internal = FALSE, overwrite = TRUE)
