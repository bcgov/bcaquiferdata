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

  # Lithology intervals record
  "flag_int_missing", "Interval missing lithologic record", "Check original paper log",
  "flag_int_overlap", "Interval which overlaps with the next or previous interval", "Check original paper log, or fix in GWELLS if obvious",
  "flag_int_gap", "Interval which has a gap between it and the next or previous interval", "Check original paper log, or fix in GWELLS if obvious",
  "flag_int_note", "Interval at the start with to/from of 0/`NA` which marks possible notes made before the lithology records", "Check original paper log",
  "flag_int_overrun", "Interval (not at the start) that has a depth of 0/`NA` to 0/`NA` (marks an interval as a possible overrun, where the notes from a previous record have overrun onto the next line)", "Check original paper log",
  "flag_int_shortform", paste0("Interval which is not the first and has a `from` of 0/`NA` and a non-missing `to`. ",
                               "Often (but not always), this indicates that the record was entered in short hand, by omitting `from` and only inputing the `to`s."),
                               "Check original paper log; OR, If reasonable, fix `from` to be preceeding `to` in GWELLS",
  "flag_int_bottom", "Bottom interval with zero depth. Either because `to` is 0/`NA` or because `to` == `from`.", "Use `fix_bottom` argument in `wells_subset()` to add 1m to this bottom interval (this is the default).",
  "fix_int_bottom", "Whether the `flag_int_bottom` problem has been fixed or not (i.e. whether or not 1m has been added to the `to` bottom layer as well as to the depth of the well.", "",

  # Lithology records
  "flag_lith_overruns", "Well with at least one overrunning interval (`flag_int_overrun` with missing depths).", "Check original paper log",
  "flag_lith_nodepths", "All lithologic intervals have depths of 0/`NA` (`from` and `to`)", "Check original paper log",
  "flag_lith_intervals", "At least one flag present on at least one interval in this record", "",
  "flag_lith_missing", "No lithologic record for this well", "",

  # Lithology categories
  "flag_cat_bedrock", "Interval where Bedrock occurs with any other primary term", "Fix in GWELLS",
  "flag_pos_bedrock", "A non-bedrock category occurs *below* a bedrock category", "Fix in GWELLS",
  "flag_cat_boulders", "Interval where Boulders occur with any other primary term", "Fix in GWELLS",
  "flag_cat_missing", "No categories were extracted from the cleaned lithologic interval", "Open an issue",

  # Yield flags
  "flag_yield_mismatch", paste0("Lithology where there are both depths and yields, ",
                                "but the number of yield measures do not match up with the number of depth measures ",
                                "(thus `yield` and `depths` are NA). This only applies to Hydrostratigraphy."), "Check original paper log",
  "flag_yield_digits", "Lithology with extra digits which were not converted to a yield or depth. This only applies to Hydrostratigraphy.", "Fix in GWELLS"
)

usethis::use_data(flags, internal = FALSE, overwrite = TRUE)
