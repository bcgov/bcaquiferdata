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
