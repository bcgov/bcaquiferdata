# Copyright 2021 Province of British Columbia
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

lith_prep <- function(file = "GWELLS/lithology.csv") {

  readr::read_csv(file.path(cache_dir(), file),
                  guess_max = Inf, show_col_types = FALSE) %>%
    janitor::clean_names() %>%

    # Convert to metric
    dplyr::mutate(
      lithology_from_m = round(.data$lithology_from_ft_bgl * 0.3048, 2),
      lithology_to_m = round(.data$lithology_to_ft_bgl * 0.3048, 2),
      lithology_raw_data = stringr::str_to_lower(.data$lithology_raw_data)) %>%

    # Check for flags
    dplyr::group_by(.data$well_tag_number) %>%
    dplyr::arrange(.data$well_tag_number,
                   .data$lithology_from_m, .data$lithology_to_m) %>%
    dplyr::mutate(

      n = dplyr::n(),

      # Flag lithology where no depths
      flag_no_depths = all(.data$lithology_from_m == 0 & .data$lithology_to_m == 0),

      # Flag where possible overruns
      flag_overruns = any(.data$lithology_from_m == 0 & .data$lithology_to_m == 0),

      # Flag where show bottom unit
      flag_bottom_unit =
        .data$n > 1 & all(.data$lithology_from_m[-1] == 0) & all(.data$lithology_to_m[-1] > 0),

      # Flag overflow lithology (zero to zero)
      flag_zero_zero = .data$lithology_from_m == 0 & .data$lithology_to_m == 0,

      # Flag missing lithology
      flag_missing = is.na(.data$lithology_raw_data) |
        .data$lithology_raw_data == "")
}


#' Fix lithology descriptions
#'
#' Clean and categorize lithology descriptions into primary, secondary, tertiary
#' and final lithology categories.
#'
#' @param file Character. Lithology file name stored in cache
#' @param desc Character. Text string to convert (overrides `file`).
#'
#' @return
#' @export
#'
#' @examples
#'
#' lith_fix(desc = "sandy gravel")
#'
#' # basic spell checks
#' lith_fix(desc = "saandy gravel")
#'
lith_fix <- function(file = "lithology.csv", desc = NULL) {

  # File or text
  if(is.null(desc)) {
    lith_desc <- readr::read_csv(file.path(cache_dir(), file),
                                 guess_max = Inf, show_col_types = FALSE) %>%
      janitor::clean_names()
  } else {
    lith_desc <- data.frame(lithology_raw_data = desc)
  }

  # Initial cleanup -----------------------------------------------------------
  lith_desc <- lith_desc %>%
    # Convert to metric
    dplyr::mutate(
      lithology_raw_data = stringr::str_to_lower(.data$lithology_raw_data)) %>%
    dplyr::select("lithology_raw_data") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      lith_clean = .data$lithology_raw_data,
      # omit numbers and quotes
      lith_clean = stringr::str_remove_all(.data$lith_clean, "\\d"),
      # clean punctuation
      lith_clean = lith_replace(.data$lith_clean, "[^\\W]&[^\\W]", " & "),
      lith_clean = lith_replace(.data$lith_clean, "\\W", " "),
      # clean extra spaces
      lith_clean = stringr::str_squish(.data$lith_clean))


  # Define 'good' terms ---------------------
  # Add terms to keep here, lists are of the main term plus all the other terms
  # that will be consolidated into the main term.
  #
  # This means that the names(terms_good_XXX) represent the actual 'good' terms
  # the other terms are real terms (acronyms or short forms but not spelling
  # mistakes) that we will consolidate into the main good term
  #
  # NOTE: This is not a place to fix spelling! That happens later.

  # ASK: bands, stratified

  terms_good_joins <- list(# ONLY here allowed spelling alts.
    "&" = c("and", "aand", "andj", "ans", "anda", "anf"),
    "with" = c("w(?!.b.)",  # Do not match ".b." ahead of w (w.b. are waterbearing)
               "lots of", "some", "streaks of",
               "layered", "layerd", "layers of", "layers in", "layer of",
               "bands of", "lenses of", "intermittent", "swith", "withj",
               "jwith", "withs"),
    "traces" = c("trace of", "traces of", "trace", "traces"),
    "layers" = c("layers", "lenses"), # To fix later (in Categorization)
    "seams" = "seams",                # To fix in Compound terms
    "seams of" = "seams of")          # To fix in Compound terms

  # Terms that need to be pulled out before the main ones
  terms_good_first <- list("bedrock" = c("rock", "solid rock")) # Otherwise becomes gravel

  terms_good_main <- list("clay" = "clay",
                          "silt" = c("muck", "mud"),
                          "sand" = "sand",
                          "gravel" = c("stone", "cobble", "pebble",
                                       "rocks", "stones", "cobbles", "pebbles",
                                       "grav", "grvl", "cobl", "peagravel",
                                       "pea gravel"),
                          "till" = c("blue clay", "blue c"),
                          "sgtill" = "sgtill",
                          "boulders" = c("boulder", "bldrs"))

  terms_good_main_y <- list(
    "clayey" = "clayish",
    "silty" = c("mucky", "muddy", "dirty", "silted"),
    "sandy" = "sandy",
    "gravely" = c("rocky", "stoney", "cobbly", "pebbly", "gravelly", "graveled"),
    "tilly" = "tilly",
    "bouldery" = "bouldery")

  terms_good_org <- list(
    "organic" = c(
      "soil", "loam", "topsoil", "dirt",  # Soils
      "organic", "vegetation", "vegetable matter", "veg matter",
      "roots", "rootlets", "stump",
      "peat", "wood"))

  terms_good_sgtill <- list(
    "sgtill" = "sgtill",
    "compact" = c("cemented", "compact", "compacted", "hardpacked", "packed"),
    "hardpan" = c("hardpan", "hard pan"))

  terms_good_bedrock <- list(
    "bedrock" = c("andesite", "argillite",
                  "basalt", "basaltic", "bentonite",
                  "bedrock", "calcite",
                  "claystone", "chert", "coal",
                  "conglomerate",
                  "dolomite", "feldspar",
                  "gneiss", "granite","greenstone", "igneous",
                  "lava", "limestone", "marl", "mudstone", "porphry",
                  "sandstone", "schist", "sedimentary", "shale", "siltstone",
                  "slate", "soapstone",
                  "quartz", "quartzite",
                  "volcanic"))
  names(terms_good_bedrock)[names(terms_good_bedrock) == ""] <-
    terms_good_bedrock[names(terms_good_bedrock) == ""] # Names where none

  terms_good_bedrock_desc <- list(
    "weathered" = "weathered",
    "fractured" = c("broken", "fracturing", "fracs", "fracture", "fractures",
                    "fragments", "fragmented", "rotten", "caving",
                    "shattered"),
    "faulted" = c("fault", "faulty", "cracks"))

  # Catch for other, non-main lithology terms
  terms_good_other <- list("shells" = c("seashell", "clamshell"),
                           "overburden" = "overburden",
                           "hard earth" = "hard earth")

  # Relevant terms related to Aquifers, but not for lithology
  terms_good_extra <- list("aquifer" = "aquifer",
                           "water-bearing" = c("waterbearing", "wb", "w\\.b\\."),
                           "flow" = c("flowing", "stream of water", "water"),
                           "trickle" = "trickle",
                           "seepage" = "seepage",
                           "wet" = "wet",
                           "saturated" = "saturated",
                           "artesian" = "artesian",
                           "reservoir" = "reservoir")


  terms_good_yield <- list("gpm" = c("usgpm", "us gmp", "i gpm"),
                           "gph" = c("usgph", "us gph"))


  terms_good <- c(
    # Joins
    terms_good_joins,  #'&' included again (below) because symbol, not a word
    # need to be first
    terms_good_first,
    # main terms
    terms_good_main,
    # -y terms
    terms_good_main_y,
    # special
    terms_good_sgtill,
    # organics
    terms_good_org,
    # bedrock
    terms_good_bedrock,
    # bedrock descriptors
    terms_good_bedrock_desc,
    # other
    terms_good_other,
    # extra water/aquifer-related terms
    terms_good_extra,
    # yield-related terms
    terms_good_yield # Too small to check spelling
  )

  # Get terms
  lith_terms <- lith_get_terms(lith_desc$lith_clean, not = names(terms_good))

  # First Round - BASIC -----------

  # Add terms to fix spelling on here. Note that the list set up is different
  # from the 'good' terms (above).
  #
  # Here, names(terms_sp_XXX) represent the regular expression for fixing the
  # spelling, while the contents are the actual 'good' terms.
  #
  # This section first creates a number of these spelling fix lists and then
  # applies them at the end (BASIC TERMS - Apply fix).

  ## Spelling in basic terms -----
  terms_sp_basic <- c(all_terms(terms_good_main),
                      all_terms(terms_good_org)) %>%
    # No sandy, no pea, no rock(s)
    lith_fix_spelling(
      terms = lith_terms,
      omit = "(y$)|(diryt)|(\\bpea\\b)|(\\brock\\b)|(\\brocks\\b)|(\\bgood\\b)|(\\bmed\\b)")  %>%
    # Add Clay specifically because odd ending
    merge_lists(lith_fix_spelling("clay", lith_terms, omit = "yy|ey")) %>%
    # Specific spelling fixes
    merge_lists(list("pebbles" = c("peb", "pebbs", "pebb"),
                     "gravel" = c("gravesls", "gralve"),
                     "silt" = "siltys")) %>%
    lith_prep_regex()

  ## Spelling in basic "ly" terms -----
  terms_sp_basic_ly <- terms_good_main_y %>%
    all_terms() %>%
    lith_fix_spelling(lith_terms, include = "y$") %>%
    merge_lists(lith_fix_spelling("clayey", lith_terms, include = "yy|ey")) %>%
    # Specific spelling fixes
    merge_lists(list("sandy" = "sandier", "silty" = "siltier")) %>%
    lith_prep_regex()

  ## Spelling in sgtill terms ----
  terms_sp_sgtill <- terms_good_sgtill %>%
    all_terms() %>%
    lith_fix_spelling(lith_terms, omit = "still") %>%
    lith_prep_regex()

  ## Spelling in bedrock terms ----
  terms_sp_bedrock <- c(terms_good_bedrock_desc, terms_good_bedrock) %>%
    all_terms() %>%
    lith_fix_spelling(
      lith_terms,
      omit = "graphite|scale|col|coat|cal|coar|coral|frass|\\brock\\b") %>%
    # Specific spelling fixes
    merge_lists(list("fractures" = c("fracutres", "fractues"),
                     "sandstone" = c("sandst", "sandsto", "sandsome", "sandstn"),
                     "schist" = "shst"
    )) %>%
    lith_prep_regex()

  ## Spelling in other terms ------------------
  terms_sp_other <- c(terms_good_other, terms_good_first) %>%
    all_terms() %>%
    lith_fix_spelling(lith_terms, omit = "shelfs|(\\brock\\b)|(\\brocks\\b)") %>%
    lith_prep_regex()


  ## Spelling in extra terms ------------------
  terms_sp_extra <- terms_good_extra %>%
    all_terms() %>%
    # don't look for spelling issues in wet or wb (too small)
    .[!. %in% c("wet", "wb", "w\\.b\\.")] %>%
    lith_fix_spelling(lith_terms, omit = "slowing|low|slow|blow") %>%
    lith_prep_regex()

  ## Consolidate main terms ------
  terms_consolidate <- c(terms_good_first,
                         terms_good_main, terms_good_main_y,
                         terms_good_org, terms_good_sgtill,
                         terms_good_bedrock, terms_good_bedrock_desc,
                         terms_good_other, terms_good_extra,
                         terms_good_yield) %>%
    lith_prep_regex()

  ## Missing spaces in basic terms -----
  # fix known multi-term problems (i.e. where terms should be split or combined)

  terms_multi <- c(names(terms_good_main),
                   names(terms_good_main_y),
                   names(terms_good_org),
                   names(terms_good_bedrock)) %>%
    stringr::str_c(collapse = "(s*)|") %>%
    stringr::str_c("(", ., ")") %>%
    list("\\1 \\2" = stringr::str_c(., .),
         "\\1 and" = stringr::str_c(., "and"),
         "and \\1" = stringr::str_c("and", .),
         "\\1 and \\2" = stringr::str_c(., "and", .),
         "with \\1" = stringr::str_c("with", .),
         "\\1 with" = stringr::str_c(., "with"),
         "\\1 with\\2" = stringr::str_c(., "with", .),
         "bearing gravel" = "bearinggravel",
         "clean gravel" = "cleangravel",
         "sandstone layers" = "sandstonelayers",
         "coarse sand" = "coarsesand",
         "fine sand" = "finesand", "hard sand" = "hardsand",
         "hard shale" = "hardshale", "grey sand" = "greysand",
         "black mud" = "blackmud", "black sand" = "blacksand",
         "grey sand" = "greysand", "coarse gravel" = "coarsegravel",
         "brown sandy" = "brownsandy", "brown silty" = "brownsilty",
         "gravel brown" = "gravelbrown",
         "bouldery fill" = "boulderyfill", "shaley mica" = "shaleymica",
         "shaley sandstone" = "shaleysandstone",
         "water-bearing" = "water bearing", "wb" = "w b") %>%
    .[-1] %>%
    lith_prep_regex()

  ## BASIC TERMS - Apply fix ----
  # Note that order is order of priority

  lith_desc <- lith_desc %>%
    dplyr::mutate(

      # Consolidate joins
      lith_clean = lith_replace(.data$lith_clean,
                                lith_prep_regex(terms_good_joins)),

      # Fix spellings (order is order of priority)
      lith_clean = lith_replace(.data$lith_clean, terms_sp_basic),
      lith_clean = lith_replace(.data$lith_clean, terms_sp_basic_ly),
      lith_clean = lith_replace(.data$lith_clean, terms_sp_sgtill),
      lith_clean = lith_replace(.data$lith_clean, terms_sp_bedrock),
      lith_clean = lith_replace(.data$lith_clean, terms_sp_other),
      lith_clean = lith_replace(.data$lith_clean, terms_sp_extra),

      # Consolidate terms
      lith_clean = lith_replace(.data$lith_clean, terms_consolidate),

      # Fix multi-terms
      lith_clean = lith_replace(.data$lith_clean, terms_multi),

      # Clean up
      lith_clean = stringr::str_squish(.data$lith_clean))

  # Get terms again
  lith_terms <- lith_get_terms(lith_desc$lith_clean, not = names(terms_good))

  # Second Round - FIDDLY ----------------

  # This section works by fixing more fiddly details. For example, consolidating
  # various term combinations into sgtill, or consolidating other known compound
  # terms.
  #
  # The format of these lists is similar to the spelling fix lists above:
  # names(terms_XXXX) represents the regular expression to match, contents the
  # term to end up with.

  ## Consolidate compound terms -----

  # NOTE: Only applies where need to specify what comes before
  #  (i.e. "sand layers" = "with sand"). If you can do a drop in replacement
  # (i.e. "band of" = "with", then add to terms_good_joins in
  # "Define 'good' terms" (above)

  terms_compound <- c(
    "& \b(silt|clay|sand|gravel)\b" = "with \\1",  # i.e., with sand
    "\b(silt|clay|sand|gravel) layer(s*)(es*)\b" = "with \\1",
    "\b(silt|clay|sand|gravel) streak(s*)\b" = "with \\1",
    "\b(silt|clay|sand|gravel) band(s*)\b" = "with \\1",
    "\b(silt|sand|gravel) seam(s*)\b" = "\\1y",  #i.e. sandy
    "\bclay seam(s*)\b" = "clayey",
    "\bsilt sand\b" = "silty sand")

  ## FIDDLY TERMS - Apply fix ----
  lith_desc <- lith_desc %>%
    dplyr::mutate(
      lith_clean = lith_replace(.data$lith_clean, terms_compound),
      lith_clean = stringr::str_squish(.data$lith_clean))

  # Get terms again
  lith_terms <- lith_get_terms(lith_desc$lith_clean, not = names(terms_good))


  # Finalize --------------------

  ## Terms to ignore ------------
  # Note, that all terms not in 'terms_good' are ignored.
  # THESE terms are ignored in the lith_terms.csv output file so they don't
  # need to be further considered.

  terms_omit <- list(
    "purple", "red", "orange", "yellow", "green", "blue", "white",
            "black", "grey", "brown", "tan", "turquoise", "rust", "brick",
            "pink") %>%
              append(., paste0(., "ish")) %>%
    append(list(
      "colour", "dark", "light", "pale", "hard", "soft", "softer", "heavily",
      "fine", "small", "medium", "med", "large", "coarse",
      "packed", "tight", "tightly", "loose", "firm", "sticky", "clean", "gummy",
      "little", "lots", "extreme", "big", "less", "more", "thick", "thin",
      "very",
      "feet", "foot", "ft",
      "wet", "water", "damp", "dry", "wb", "moist", "moisture",
      "br", "bear", "bearing", "dug", "ball",
      "open", "hole", "interbedded", "thin", "dissolve"
    )) %>%
    purrr::map(~lith_fix_spelling(.,
                                  str_dist = dplyr::if_else(nchar(.) > 4, 3, 2),
                                  terms = lith_terms)) %>%
    unlist() %>%
    unique() %>%
    .[!. %in% terms_good]


  ## Output remaining terms ---------------
  # lith_terms <- dplyr::select(lith_desc, "lith_clean") %>%
  #   dplyr::distinct() %>%
  #   dplyr::mutate(lith_clean = stringr::str_split(.data$lith_clean, pattern = "\\b")) %>%
  #   tidyr::unnest("lith_clean") %>%
  #   dplyr::filter(!.data$lith_clean %in% c(names(terms_good), terms_omit,
  #                                          "", " ", " & ", "and")) %>%
  #   dplyr::count(.data$lith_clean) %>%
  #   dplyr::arrange(dplyr::desc(.data$n))
  #
  # readr::write_csv(lith_terms, "lith_terms.csv")

  # Troubleshooting: Look for specific terms in the data
  #dplyr::filter(lith_desc, stringr::str_detect(lith_clean, "sandand"))


  ## Remove all non 'good' terms --------------
  lith_desc2 <- lith_desc %>%
    dplyr::mutate(
      lith_clean = stringr::str_extract_all(
        .data$lith_clean,
        stringr::str_c(
          "&|", # Don't use \b (boundaries) on & or won't match
          stringr::str_c("\\b", names(terms_good), "\\b",collapse = "|"))),
      lith_clean = purrr::map_chr(.data$lith_clean, paste, collapse = " "))

  # Remove orphaned connectors --------------
  # single with/& or at ends
  orphaned <- "(^with$)|(^\\bwith )|( with\\b$)|(^&$)|(^& )|( &$)"
  orphaned <- stringr::str_c(orphaned, "|(^layers$)")

  lith_desc2 <- lith_desc2 %>%
    dplyr::mutate(
      # Run twice in case we have "with & sand" for example
      lith_clean = stringr::str_remove(.data$lith_clean, orphaned),
      lith_clean = stringr::str_remove(.data$lith_clean, orphaned))

  ## Remove repetitions of a good term ----------
  terms_dup <- stringr::str_c("\\b", names(terms_good), "( (& )*",
                              names(terms_good), "\\b)+") %>%
    stats::setNames(names(terms_good), .)

  lith_desc2 <- lith_desc2 %>%
    dplyr::mutate(
      lith_clean = lith_replace(.data$lith_clean, terms_dup))


  # Categorizing lithology ----------------------------------------------

  # Terms that might be lith but we want to catch them
  terms_extra_flags <- list("shells", "boulders", "organics") %>%
    setNames(., .)

  lith_cats <- lith_desc2 %>%
    dplyr::select("lith_clean") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Get primary/secondary/tertiary terms
      primary = lith_primary(
        .data$lith_clean, terms_good = c(terms_good_main, terms_good_bedrock,
                                         terms_good_bedrock_desc,
                                         terms_good_org, terms_good_other,
                                         terms_good_extra, terms_good_sgtill)),

      secondary = lith_secondary(
        .data$lith_clean, terms_good = c(terms_good_main, terms_good_bedrock,
                                         terms_good_bedrock_desc,
                                         terms_good_org, terms_good_other,
                                         terms_good_extra, terms_good_sgtill)),

      tertiary = lith_tertiary(.data$lith_clean, terms_good_main_y),

      # Flag combinations that should be inspected
      flags = purrr::pmap(
        list(.data$primary, .data$secondary, .data$tertiary), lith_flag),

      # Move (some) extra terms from categories to extra / yield
      extra = lith_cat_terms(
        .data$lith_clean,
        terms_good = append(terms_good_extra, terms_extra_flags)),
      yield = lith_cat_terms(.data$lith_clean, terms_good = terms_good_yield),
      primary = purrr::map(.data$primary, ~.[!. %in% names(terms_good_extra)]),

      # Apply categorizing based on primary/secondary/tertiary
      lithology_category = purrr::pmap_chr(list(.data$primary,
                                                .data$secondary,
                                                .data$tertiary),
                                      lith_categorize)) %>%
    tidyr::unnest("flags")

  # For comparing
  lith_combo <- dplyr::left_join(lith_desc2, lith_cats, by = "lith_clean")

  lith_combo %>%
    dplyr::mutate(
      lith_primary = purrr::map_chr(.data$primary, collapse_nested),
      lith_secondary = purrr::map_chr(.data$secondary, collapse_nested),
      lith_tertiary = purrr::map_chr(.data$tertiary, collapse_nested),
      lithology_extra = purrr::map_chr(.data$extra, collapse_nested),
      yield_units = purrr::map_chr(.data$yield, collapse_nested)) %>%
    # Bind to lithology data and return
    dplyr::select("lithology_raw_data", "lithology_clean" = "lith_clean",
                  "lith_primary", "lith_secondary", "lith_tertiary",
                  "lithology_extra", "lithology_category", "yield_units",
                  dplyr::starts_with("flag_"))
}

lith_yield <- function(lith, flatten = FALSE) {

  p_units_yield <- "( )?(gpm|gph)"
  p_units_depth_ft <- "'|ft|feet"
  p_units_depth_m <- "(m|meters|metres)\\b"
  p_units_depth <- paste0("( )?(", p_units_depth_ft, "|", p_units_depth_m, ")")

  p_yield <- paste0("(", p_range(), "|", p_dbl(), ")", p_units_yield)
  p_depth <- paste0("\\d+", p_units_depth)

  l <- lith %>%
    dplyr::filter(.data$yield_units != "") %>%
    dplyr::select("lithology_raw_data") %>%
    dplyr::mutate(
      flag_extra_digits = stringr::str_squish(.data$lithology_raw_data),
      flag_extra_digits = fix_fraction(.data$flag_extra_digits),
      yield_chr = stringr::str_extract_all(
        .data$flag_extra_digits, .env$p_yield),
      flag_extra_digits = stringr::str_remove_all(
        .data$flag_extra_digits, .env$p_yield),
      yield_chr = purrr::map(.data$yield_chr,
                             ~stringr::str_remove_all(., .env$p_units_yield)),
      depth = stringr::str_extract_all(.data$flag_extra_digits, .env$p_depth),
      flag_extra_digits = stringr::str_remove_all(
        .data$flag_extra_digits, .env$p_depth),
      flag_extra_digits = stringr::str_extract_all(
        .data$flag_extra_digits, "\\d+"),
      flag_extra_digits = purrr::map_chr(
        .data$flag_extra_digits, ~paste0(.x, collapse = ";")),
      depth_units = purrr::map_chr(
        .data$depth,
        ~ stringr::str_extract_all(.x, .env$p_units_depth) %>%
          stringr::str_replace_all(c(setNames("m", p_units_depth_m),
                                     setNames("ft", p_units_depth_ft))) %>%
          stringr::str_trim() %>%
          unique() %>%
          paste0("")),
      depth = purrr::map(
        .data$depth,
        ~stringr::str_remove_all(.x, .env$p_units_depth) %>% as.numeric()),
      yield = purrr::map(.data$yield_chr, fix_range)) %>%
    dplyr::select(-"yield_chr")

  dplyr::left_join(lith, l, by = "lithology_raw_data") %>%
    dplyr::relocate("yield_units", .after = "yield")
}



lith_get_terms <- function(from, not) {
  unique(from) %>%
    stringr::str_split(., pattern = "\\b") %>%
    unlist() %>%
    unique() %>%
    .[!. %in% c(not, "", " ", " & ")] %>%
    stats::na.omit()
}

lith_fix_spelling <- function(to_fix, terms, str_dist = 2, omit = NULL,
                              include = NULL) {
  t <- to_fix %>%
    stats::setNames(., .) %>%
    purrr::map(~terms[stringdist::stringdist(., terms) < str_dist])
  if(!is.null(omit)) {
    t <- purrr::map(t, ~stringr::str_subset(., omit, negate = TRUE))
  } else if(!is.null(include)) {
    t <- purrr::map(t, ~stringr::str_subset(., include))
  }

  t[purrr::map(t, length) > 0] # Omit those with no corrections
}

lith_prep_regex <- function(fix, noname = FALSE) {
  if(length(fix) == 0) {
    fix <- NULL
  } else {
    if(!is.list(fix)) fix <- list(fix)
    fix <- purrr::map_chr(fix, ~stringr::str_c("\\b", ., "\\b", collapse = "|"))
    if(!noname) fix <- stats::setNames(names(fix), fix)
  }
  fix
}


lith_find <- function(x, word, verbose = FALSE) {
  if(verbose) message(stringr::str_c("\\b", word, "\\b", collapse = "|"))
  stringr::str_detect(x, stringr::str_c("\\b", word, "\\b", collapse = "|"))
}

lith_combo <- function(x, terms) {
  term <- expand.grid(terms, terms) %>%
    dplyr::filter(.data$Var1 != .data$Var2) %>%
    dplyr::summarize(term = stringr::str_c("\\b", .data$Var1, " & ", .data$Var2, "\\b",
                                           collapse = "|")) %>%
    dplyr::pull(.data$term)
  stringr::str_detect(x, term)
}

lith_replace <- function(terms, pattern, replacement) {

  if(!is.null(pattern)) {
    terms <- stringr::str_replace_all(terms, pattern, replacement)
  }
  terms
}

lith_primary <- function(terms, terms_good) {

  # Get all primary terms (i.e. good terms, not 'y' and do not have a 'with' before
  names(terms_good) %>%
    paste0("(?<!with )", .) %>%   # Cannot have a 'with ' right before the term
    lith_prep_regex(noname = TRUE) %>%
    stringr::str_extract_all(terms, pattern = .)

  # # Get all combo primary terms
  # # (i.e. "sand & gravel", "clay & sand", "clay & sand & silt", etc.)
  # p <- stringr::str_extract_all(terms, "\\w+(?= &)|(?<=& )\\w+") %>%
  #   purrr::map(~sort(stringr::str_to_title(.))) %>%
  #   purrr::map_chr(~stringr::str_c(., collapse = " and "))
  #
  # # Get all solo primary terms
  # # (i.e. "sand", "sand with clay", "clayey sand" would all be "sand")
  # p_solo <- stringr::str_extract_all(terms,
  #                                    "^\\w+$|\\w+(?= with)|(?<=[etdlr]{1}+y )\\w+") %>%
  #   purrr::map(~.[. %in% names(c(terms_good_main,
  #                                terms_good_bedrock,
  #                                terms_good_org,
  #                                terms_good_extra))]) %>%
  #   purrr::map_chr(~stringr::str_c(stringr::str_to_title(.), collapse = ""))
  #
  # p[p == ""] <- p_solo[p == ""]
  #p
}

lith_secondary <- function(terms, terms_good) {
  # Get all secondary terms
  # i.e. good terms, no "y", but "with " before
  #      OR for bedrock, a descriptive term like fractured
  with <- names(terms_good) %>%
    paste0("(?<=with )", .) %>%   # Must have a 'with ' right before the term
    lith_prep_regex(noname = TRUE)
#
#   # Create regex for matching bedrock terms
#   desc <- names(terms_good_bedrock) %>%
#     lith_prep_regex(noname = TRUE)
#
#   # Grab descriptive terms that appear before the bedrock term
#   desc <- names(terms_good_bedrock_desc) %>%
#     stringr::str_c(collapse = "|") %>%
#     stringr::str_c("(", ., ")(?= (", desc, "))")

  stringr::str_extract_all(terms, pattern = paste0("(", with, ")"))
}

lith_tertiary <- function(terms, terms_good) {
  # Get all tertiary terms (i.e. good terms ending in "y"
  names(terms_good) %>%
    lith_prep_regex(noname = TRUE) %>%
    stringr::str_extract_all(terms, pattern = .) %>%
    # Remove y ends
    purrr::map(~lith_replace(., c("clayey" = "clay",
                                  "silty" = "silt",
                                  "sandy" = "sand",
                                  "gravely" = "gravel",
                                  "tilly" = "till",
                                  "bouldery" = "boulders")))
}

lith_cat_terms <- function(terms, terms_good) {
  names(terms_good) %>%
    lith_prep_regex(noname = TRUE) %>%
    stringr::str_extract_all(terms, pattern = .)
}

lith_categorize <- function(p, s, t) {

  dirty <- any(c(s, t) %in% c("silt", "clay"))
  gravelly <- any(c(s, t) %in% c("sand", "gravel"))
  sg_till <- any(c(p, s) %in% "sgtill")
  any_gravel <- any(c(p, s, t) %in% "gravel")
  any_sand <- any(c(p, s, t) %in% "sand")

  cat <- NA_character_

  # Bedrock
  if(any(c(p, s, t) %in% c("weathered", "fractured", "faulted"))) {
    cat <- "Weathered, Fractured or Faulted Bedrock"
  } else if(any(c(p, s, t) %in% "bedrock")) {
    cat <- "Bedrock"

  # Boulders
  } else if("boulders" %in% c(p, s, t)) {
    cat <- "Boulders"

    # Sand, Gravel and fines
  } else if(all(c("sand", "gravel") %in% p) & dirty) {             # Sand and Gravels
    cat <- "Sand and Gravel (Dirty)"

  } else if((any(p %in% "sand") & dirty) |
            all(c("sand", "silt") %in% p)) {                   # Sand and Fines
    cat <- "Sand and Fines"

  } else if(("sand" %in% p & any_gravel) |
            ("gravel" %in% p & any_sand)) {
    cat <- "Sand and Gravel (Clean)"

  } else if(any(p %in% "gravel") & dirty |
            all(c("gravel", "silt") %in% p)){               # Gravel (dirty)
    cat <- "Gravel (Dirty)"

    # Sand or Gravel Till or Diamicton
  } else if(sg_till |
            (any(p %in% c("till", "clay")) & (any_sand | any_gravel))) {
    cat <- "Sand or Gravel Till or Diamicton"
  } else if(any(c("sand", "gravel") %in% p) & "till" %in% c(s, t)) {
    cat <- "Sand or Gravel Till or Diamicton"
  } else if("compact" %in% p & (any(c("sand", "gravel") %in% c(p, s)))) {
    cat <- "Sand or Gravel Till or Diamicton"

  } else if(gravelly & "silt" %in% p) {                               # Silts
    cat <- "Sandy or Gravelly Silt"

    # Clay and Till
  } else if(any(p %in% c("till", "hardpan", "hard earth")) |
            (any(c("silt", "clay") %in% p) & "till" %in% t)) {
    cat <- "Medium to Clay Till or Diamicton"
  } else if("compact" %in% p & any(c("silt", "clay") %in% p)) {
    cat <- "Medium to Clay Till or Diamicton"
  } else if(all(c("silt", "clay") %in% unique(c(p, t, s))) ) {
    cat <- "Medium to Clay Till or Diamicton"

    # Organics
  } else if("organic" %in% p) {
    cat <- "Organics"

    # If one primary, becomes category (or if primary with shells)
  } else if(length(p) == 1){
    cat <- stringr::str_to_title(p)
  } else if(length(p[!p %in% c("shells")]) == 1) {
    cat <- stringr::str_to_title(p[!p %in% c("shells")])

    # If only one of mains left, apply as category
  } else if(length(unique(c(p, s, t))) == 1 &
            any(c("gravel", "silt", "sand", "clay") %in% c(p, s, t))) {
    cat <- stringr::str_to_title(unique(c(p, s, t)))
  }

  # Fix formatting
  if(!is.na(cat) && cat == "Sgtill") cat <- "SG Till"

  cat
}

# Flag combinations that the user should look at
lith_flag <- function(p, s, t) {

  bedrock <- c("bedrock", "faulted", "fractured", "weathered")

  dplyr::tibble(
    flag_bedrock = any(bedrock %in% p) & length(p) > 1 & !all(bedrock %in% p),
    flag_boulders = "boulders" %in% p & length(p) > 1,
    flag_missing_cats = all(is.na(c(p, s, t)))
  )
}

collapse_nested <- function(x) {
  if(length(x) > 0) paste0(x, collapse = ", ") else ""
}


all_terms <- function(terms) {
  c(names(terms), unlist(terms)) %>%
    stats::setNames(nm = NULL) %>%
    unique()
}

merge_lists <- function(l1, l2) {
  for(n in names(l2)) {
    l1[[n]] <- c(l1[[n]], l2[[n]])
  }
  l1
}
