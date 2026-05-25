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

# fmt:skip
flags <- dplyr::tribble(
  ~"Flag", ~"Description", ~"Solution",

  # Lithology intervals record -------------------------------------------------
  "flag_int_missing",
  "Interval is missing lithologic record.", 
  "Check original paper log",

  "flag_int_overlap", 
  "Interval overlaps the next or previous interval.", 
  "Check original paper log, or fix in GWELLS if obvious",

  "flag_int_gap", 
  "Interval has a gap between it and the next or previous interval.", 
  "Check original paper log, or fix in GWELLS if obvious",

  "flag_int_note", 
  "This first depth interval has depth values both to and from 0 (or `NA`) which marks possible notes made before the lithology records.", 
  "Check original paper log",

  "flag_int_overrun", 
  "Second or later depth interval has depth values both to and from 0 (or `NA). This marks an interval as a possible overrun, where the notes from a previous record have overrun onto the next line.", 
  "Check original paper log",
  
  "flag_int_shortform", 
  "Second or later depth interval has a `from` of 0 (or `NA`) and a non-zero (and non-missing) `to`. Often (but not always), this indicates that the record was entered in short hand, by omitting `from` and only inputing the `to`s.",
  "Check original paper log; OR, If reasonable, fix `from` to be preceeding `to` in GWELLS",
  
  "flag_int_bottom", 
  "Bottom interval with zero depth. Either because `to` is 0 (or `NA`) or because `to` == `from`.", 
  "Use `fix_bottom` argument in `wells_subset()` to add 1m to this bottom interval (this is the default).", 
  
  "fix_int_bottom", 
  "Indicates whether the `flag_int_bottom` problem has been fixed here by adding 1m to the `to` bottom layer as well as to the depth of the well.", 
  "", 

 # Lithology records --------------------------------------------------------------
 "flag_lith_overruns", 
 "Well with at least one overrunning interval (`flag_int_overrun` with missing depths).", 
 "Check original paper log", 

 "flag_lith_nodepths", 
 "All lithologic intervals have depths of 0 (or `NA`) in both `from` and `to`.", 
 "Check original paper log", 

 "flag_lith_intervals", 
 "At least one flag present on at least one interval in this record.", 
 "", 

 "flag_lith_missing", 
 "There are no lithologic records for this well.", 
 "", 

 # Lithology categories---------------------------------------------------------------
 "flag_cat_bedrock",
  "Interval where Bedrock occurs with any other primary term.", 
  "Fix in GWELLS", 

 "flag_pos_bedrock", 
 "A non-bedrock category occurs *below* a bedrock category.",
  "Fix in GWELLS", 

 "flag_cat_boulders", 
 "Interval where Boulders occur with any other primary term.",
  "Fix in GWELLS", 

 "flag_cat_missing", 
 "No categories were extracted from the cleaned lithologic interval.", 
 "Open an issue", 

 # Yield flags from lith ------------------------------------------------------
 "flag_yield_mismatch", 
 "Lithology where there are both depths and yields, but the number of yield measures do not match up with the number of depth measures (thus `yield` and `depths` are `NA`). This only applies to Hydrostratigraphy.",
 "Check original paper log", 

 "flag_yield_digits", 
 "Lithology with extra digits which were not converted to a yield or depth. This only applies to Hydrostratigraphy.", 
 "Fix in GWELLS", 

 # Well depth ----------------------------------------------------------------
 "flag_depth_missing", 
 "Well is missing depth.", 
 "Check original paper log", 

 "fix_depth_missing", 
 "Indicates whether a `flag_depth_missing` problem has been fixed here by setting Well depth to the depth of the final interval.", 
 "", 

 "flag_depth_mismatch", 
 "Well depth is not equal to the depth of the final lithology interval.",
 "Check original paper log", 

 # Well other ----------------------------------------------------------------
 "flag_yield_zero",
 "Well yield is zero, but should probablyl be missing (NA)",
 "Check original paper log",

 "fix_yield_zero",
 "Indicates whether a `flag_yield_zero` problem has been fixed by setting Well Yield of 0 to NA.",
 ""
) |>
  dplyr::select(-"Solution")

usethis::use_data(flags, internal = FALSE, overwrite = TRUE)
