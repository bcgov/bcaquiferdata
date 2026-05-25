# Flags

Flag definitions for lithology and hydrostratigraphy. You can access
this list in R at any time through the `flags` dataset.

Many of these flags indicate potential problems in the original data
record on GWELLS. However, if you believe that the record is correct,
but that the *flag* is incorrect, please [open an
issue](https://github.com/bcgov/bcaquiferdata/issues) and let us know
what the problem is (which flag is incorrect and why) and with which
Well (`well_tag_number`).

`from` and `to` fields refer to
`lithology_from_ft_bgl`/`lithology_to_ft_bgl` and
`lithology_from_m`/`lithology_to_m` fields

The middle letters (e.g., `int` in `flag_int_missing`) reflect the kind
of data being flagged.

- `int` refers to a specific lithologic depth interval
- `lith` refers to a well’s entire lithologic record
- `cat` refers to the lithology categorization
- `pos` refers to the specific position of a category
- `yield` refers to the yield extracted from a lithologic record
- `depth` refers to well depths

| Flag | Description |
|----|----|
| flag_int_missing | Interval is missing lithologic record. |
| flag_int_overlap | Interval overlaps the next or previous interval. |
| flag_int_gap | Interval has a gap between it and the next or previous interval. |
| flag_int_note | This first depth interval has depth values both to and from 0 (or `NA`) which marks possible notes made before the lithology records. |
| flag_int_overrun | Second or later depth interval has depth values both to and from 0 (or \`NA). This marks an interval as a possible overrun, where the notes from a previous record have overrun onto the next line. |
| flag_int_shortform | Second or later depth interval has a `from` of 0 (or `NA`) and a non-zero (and non-missing) `to`. Often (but not always), this indicates that the record was entered in short hand, by omitting `from` and only inputing the `to`s. |
| flag_int_bottom | Bottom interval with zero depth. Either because `to` is 0 (or `NA`) or because `to` == `from`. |
| fix_int_bottom | Indicates whether the `flag_int_bottom` problem has been fixed here by adding 1m to the `to` bottom layer as well as to the depth of the well. |
| flag_lith_overruns | Well with at least one overrunning interval (`flag_int_overrun` with missing depths). |
| flag_lith_nodepths | All lithologic intervals have depths of 0 (or `NA`) in both `from` and `to`. |
| flag_lith_intervals | At least one flag present on at least one interval in this record. |
| flag_lith_missing | There are no lithologic records for this well. |
| flag_cat_bedrock | Interval where Bedrock occurs with any other primary term. |
| flag_pos_bedrock | A non-bedrock category occurs *below* a bedrock category. |
| flag_cat_boulders | Interval where Boulders occur with any other primary term. |
| flag_cat_missing | No categories were extracted from the cleaned lithologic interval. |
| flag_yield_mismatch | Lithology where there are both depths and yields, but the number of yield measures do not match up with the number of depth measures (thus `yield` and `depths` are `NA`). This only applies to Hydrostratigraphy. |
| flag_yield_digits | Lithology with extra digits which were not converted to a yield or depth. This only applies to Hydrostratigraphy. |
| flag_depth_missing | Well is missing depth. |
| fix_depth_missing | Indicates whether a `flag_depth_missing` problem has been fixed here by setting Well depth to the depth of the final interval. |
| flag_depth_mismatch | Well depth is not equal to the depth of the final lithology interval. |
| flag_yield_zero | Well yield is zero, but should probablyl be missing (NA) |
| fix_yield_zero | Indicates whether a `flag_yield_zero` problem has been fixed by setting Well Yield of 0 to NA. |
