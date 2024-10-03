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

  ~ "Flag", ~ "Description", ~ "Solution",

  # General record
  "flag_int_missing", "Interval missing lithologic record", "Check original paper log",
  "flag_int_overlap", "Interval which overlaps with the next or previous interval", "Check original paper log, or fix in GWELLS if obvious",
  "flag_int_gap", "Interval which has a gap between it and the next or preivous interval", "Check original paper log, or fix in GWELLS if obvious",
  "flag_int_overrun", "Interval that has a depth of 0/`NA` to 0/`NA` (marks an interval as a possible overrun, where the notes from a previous record have overrun onto the next line)", "Check original paper log",
  "flag_int_shortform", paste0("Interval which is not the first nor the last but has a `from` of 0/`NA` and a non-missing `to`. ",
                               "Often (but not always), this indicates that the record was entered in short hand, by omitting `from` and only inputing the `to`s."),
                               "If reasonable, fix `from` to be preceeding `to` in GWELLS",

  "flag_overruns", "Well with at least one overrunning interval (`flag_int_overrun` with missing depths).", "Check original paper log",

  "flag_bottom_unit", paste0("Well where all `from` depths are 0/`NA` but `to` depths are generally present. ",
                             "Possibly indicates that the original log shows only the bottom of a unit"), "Check original paper log",
  "flag_no_depths", "All lithologic intervals have depths of 0 or missing (`from` and `to`)", "Check original paper log",

  # Lithology categories
  "flag_bedrock", "Interval where Bedrock occurs with any other primary term", "Fix in GWELLS",
  "flag_bedrock_position", "A non-bedrock category occurs *below* a bedrock category", "Fix in GWELLS",
  "flag_boulders", "Interval where Boulders occur with any other primary term", "Fix in GWELLS",
  "flag_missing_cats", "No categories were extracted from the cleaned lithologic interval", "Check original paper log",

  # Yield flags
  "flag_yield", paste0("Lithology where there are both depths and yields, ",
                       "but the number of yield measures do not match up with the number of depth measures ",
                       "(thus `yield` and `depths` are NA). This only applies to Hydrostratigraphy."), "Check original paper log",
  "flag_extra_digits", "Lithology with extra digits which were not converted to a yield or depth. This only applies to Hydrostratigraphy.", "Fix in GWELLS"
)

usethis::use_data(flags, internal = FALSE, overwrite = TRUE)
