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

# Use "lithologic interval" rather than "lithology record" - 2023-09-15
flags <- dplyr::tribble(

  ~ "Flag", ~ "Description",

  # General record
  "flag_missing", "Missing lithologic interval",
  "flag_no_depths", "All lithologic intervals have depths of 0 ('from' and 'to')",
  "flag_zero_zero", "Any lithologic interval that has a depth of 0 to 0 (only marks a single interval)",
  "flag_overruns", paste0("Any lithologic interval has both depths of 0 ('from' and 'to'). ",
                          "Possibly a second entry for a single interval, 'overrun' interval ",
                          "(marks all intervals for a well)"),

  "flag_bottom_unit", paste0("All 'from' depths are 0, end depths are not (except possibly the first).",
                             "Problem: The original log shows only the bottom of a unit (fix in GWELLS)"),

  # Lithology catetegories
  "flag_bedrock", "Interval where Bedrock occurs with any other primary term",
  "flag_bedrock_position", "A non-bedrock category occurs *below* a bedrock category",
  "flag_boulders", "Interval where Boulders occur with any other primary term",
  "flag_missing_cats", "No categories were extracted from the cleaned lithologic interval",
  "flag_extra_digits", "Lithology with extra digits which were not converted to a yield or depth (only applies to Hydrostratigraphy)"
)

usethis::use_data(flags, internal = FALSE, overwrite = TRUE)
