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



#' Prepare raw GWELLS lithology for cleaning
#'
#' @param file Character. Relative location of the downloaded data
#'
#' @return Data frame of cleaned and prepared lithology records.
#'
#' @examples
#' lith_prep(file.path(cache_dir(), "GWELLS/lithology.csv"))
#'
#' @noRd
lith_prep <- function(file = NULL) {
  if(is.null(file)) file <- file.path(cache_dir(), "GWELLS/lithology.csv")

  file %>%
    readr::read_csv(guess_max = Inf, show_col_types = FALSE, progress = FALSE) %>%
    janitor::clean_names() %>%

    # Replace NA with 0 -
    # TODO: Here treat zeros and NAs the same... Okay?
    dplyr::mutate(dplyr::across(dplyr::matches("from|to"), \(x) tidyr::replace_na(x, 0))) %>%

    # Find duplicates and log them
    lith_duplicates() %>%

    # Convert to metric
    convert_m(cols = c("lithology_from_m" = "lithology_from_ft_bgl",
                       "lithology_to_m" = "lithology_to_ft_bgl"))  %>%

   # Collect and combine lithology descriptions
    lith_desc_combine() %>%

    # Arrange and label intervals
    dplyr::arrange(.data$well_tag_number,
                   .data$lithology_from_m, .data$lithology_to_m) %>%
    dplyr::mutate(lith_n = dplyr::n(),
                  lith_rec = dplyr::row_number(),
                  .by = "well_tag_number") %>%

    # Create flags
    lith_flags_interval() %>%
    lith_flags_well()
}



# Convert to metric
convert_m <- function(data, cols, digits = 2) {
  dplyr::mutate(data,
                dplyr::across(.cols = dplyr::all_of(cols),
                              .fns = \(x) round(x * 0.3048, digits),
                              .names = "{names(cols)}"))
}

lith_desc_combine <- function(lith) {
  dplyr::mutate(
    lith,
    dplyr::across(.cols = c(
      "lithology_raw_data",
      "lithology_description_code",
      "lithology_material_code"),
      ~ as.character(.x) %>%
        tidyr::replace_na("") %>%
        stringr::str_to_lower())) %>%
    dplyr::mutate(
      lithology_raw_combined = paste(
        .data$lithology_raw_data,
        .data$lithology_description_code,
        .data$lithology_material_code),
      lithology_raw_combined = stringr::str_squish(.data$lithology_raw_combined))
}

lith_duplicates <- function(lith) {

  ## Remove Exact duplicates
  dups <- which(duplicated(lith))
  if(length(dups) > 0) {
    dups_tags <- c("Wells with omitted duplicated lithology records: ",
                   unique(lith$well_tag_number[dups]))
    lf <- paste0("log_duplicate_records_", Sys.Date(), ".txt")
    writeLines(dups_tags, lf)
    message("Omitting duplicate lithology records for ",
            length(dups_tags), " wells.\nSee ", lf, " for the list of wells.")

    lith <- lith[-dups, ]
  }

  ## Find exact duplicates of whole lithology records (everything except well number)
  # TODO: What should we do with these? Omit them? Or alert users to have them
  #  fixed?

  # By lithology record only - 4025 duplicate record groups
  # d <- tidyr::nest(lith, record = -"well_tag_number")
  # dd1 <- d %>%
  #   dplyr::group_by(record) %>%
  #   dplyr::summarize(n = dplyr::n()) %>%
  #   dplyr::filter(n > 1) %>%
  #   dplyr::mutate(dup_group = dplyr::row_number()) %>%
  #   dplyr::left_join(d, by = "record")
  #
  # # By lithology and locations - 42 exact duplicates including lat/lon with different well number
  # d <- dplyr::left_join(lith,
  #                       dplyr::select(data_read("wells"), "well_tag_number",
  #                                     "longitude_decdeg", "latitude_decdeg"),
  #                       by = "well_tag_number") %>%
  #   tidyr::nest(record = -"well_tag_number")
  # dd2 <- d %>%
  #   dplyr::group_by(record) %>%
  #   dplyr::summarize(n = dplyr::n()) %>%
  #   dplyr::filter(n > 1) %>%
  #   dplyr::mutate(dup_group = dplyr::row_number()) %>%
  #   dplyr::left_join(d, by = "record")
  #
  # TODO: Flag these wells? Have a user fix them?
  # e.g., dplyr::filter(data_read("wells"), well_tag_number %in% c(57053, 79230)) |> as.data.frame()

  lith
}


lith_flags_interval <- function(lith) {
  message("Flagging problems in lithology intervals")

  dplyr::mutate(
    lith,

    # Flag individual, possible overruns - No `from` & No `to` when text takes up multiple record slots
    flag_int_overrun = .data$lithology_from_m == 0 & .data$lithology_to_m == 0 & .data$lith_rec != 1,

    # Flag notes
    flag_int_note = .data$lithology_from_m == 0 & .data$lithology_to_m == 0 & .data$lith_rec == 1,

    # Flag overlapping intervals - Non-first `from` < preceeding `to`
    flag_int_overlap = .data$lithology_from_m != 0 & .data$lithology_from_m < dplyr::lag(.data$lithology_to_m, default = -Inf) & .data$lith_rec != 1,
    flag_int_overlap = .data$flag_int_overlap | dplyr::lead(.data$flag_int_overlap, default = FALSE),

    # Flag gaps between intervals - Non-first `from` > preceeding `to`
    flag_int_gap = .data$lithology_from_m != 0 &
      !dplyr::lag(.data$flag_int_overrun, default = FALSE) &
      .data$lithology_from_m > dplyr::lag(.data$lithology_to_m, default = Inf) &
      .data$lith_rec != 1,
    flag_int_gap = .data$flag_int_gap | dplyr::lead(.data$flag_int_gap, default = FALSE),

    # Flag intermediate intervals with `from` == 0
    flag_int_shortform = !.data$flag_int_overrun & .data$lith_rec != 1 & # .data$lith_rec != .data$lith_n &
      !dplyr::lag(.data$flag_int_note, default = FALSE) &
      (.data$lithology_from_m == 0 | is.na(.data$lithology_from_m)),

    # Flag no thickness thick bottom intervals
    flag_int_bottom =
      # Last (bottom) record
      .data$lith_n == .data$lith_rec &
      # Either only one record, or not missing the 'from'
      (.data$lith_n > 1 | .data$lithology_from_m != 0) &
      # And missing the 'to' OR 'to' equivalent to 'from'
      (.data$lithology_to_m == 0 | is.na(.data$lithology_to_m) |
         .data$lithology_from_m == .data$lithology_to_m),

    # Flag missing lithology
    flag_int_missing = is.na(.data$lithology_raw_combined) |
      .data$lithology_raw_combined == ""
  )
}

lith_flags_well <- function(lith) {
  message("Flagging problems in lithology records")

  # Flags by lithology record
  dplyr::mutate(
    lith,

    # Flag lithology where no depths
    flag_lith_nodepths = all(.data$lithology_from_m == 0) & all(.data$lithology_to_m == 0),

    # Flag record with overruns (mark the whole record if there are any)
    flag_lith_overruns = any(.data$flag_int_overrun),

    # Flag record with any interval related flags
    flag_lith_intervals = any(dplyr::pick(dplyr::starts_with("flag_int_"))),
    .by = "well_tag_number"
  )

  # NOTE: Final flag_lith_missing is applied in wells_subset() when combining with the wells data
}
