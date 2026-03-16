#' Fix the depth of the final interval if missing
#'
#' The `flag_int_bottom` flag identifies the bottom interval if it is depthless,
#' but otherwise okay. Fixing these intervals means adding 1m to them and to the
#' final depth of the well.
#'
#' @param wells_sub The subsetted wells data frame (combined with lithology)
#'
#' @return Fixed wells_sub data frame
#' @noRd
fix_bottom_intervals <- function(wells_sub, fix = TRUE) {
  if (!"fix_int_bottom" %in% names(wells_sub)) {
    wells_sub$fix_int_bottom <- FALSE
  }

  # Which wells need to be fixed and haven't been?
  w <- which(wells_sub$flag_int_bottom & !wells_sub$fix_int_bottom)
  w_pretty <- unique(wells_sub$well_tag_number[w]) |> paste0(collapse = ", ")

  if (length(w) > 0) {
    if (fix) {
      message(
        "Fixing wells with a bottom lithology interval of zero thickness: ",
        w_pretty
      )

      wells_sub$lithology_to_m[w] <- wells_sub$lithology_to_m[w] + 1
      wells_sub$lithology_to_ft_bgl[w] <- wells_sub$lithology_to_ft_bgl[w] +
        3.28084
      wells_sub$well_depth_m[w] <- wells_sub$well_depth_m[w] + 1
      wells_sub$finished_well_depth_ft_bgl[
        w
      ] <- wells_sub$finished_well_depth_ft_bgl[w] + 3.28084
      wells_sub$fix_int_bottom[w] <- TRUE
    } else {
      message(
        "Some wells have a bottom lithology interval of zero thickness.\n",
        "Consider either using `fix_bottom = TRUE` in `wells_subset()` or ",
        "fixing the original record in GWELLS\n",
        "Wells: ",
        w_pretty
      )
    }
  }

  wells_sub
}


fix_depth_missing <- function(wells_sub, fix = TRUE) {
  if (!"fix_depth_missing" %in% names(wells_sub)) {
    wells_sub$fix_depth_missing <- FALSE
  }

  # Which wells are fixable and haven't been?
  w <- wells_sub$well_tag_number[
    wells_sub$flag_depth_missing &
      !wells_sub$flag_lith_missing &
      !wells_sub$fix_depth_missing
  ] |>
    unique()
  w_pretty <- paste0(w, collapse = ", ")

  if (length(w) > 0) {
    if (fix) {
      message("Fixing wells missing depth: ", w_pretty)

      w <- wells_sub %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(.data$well_tag_number %in% .env$w) %>%
        dplyr::mutate(
          well_depth_m = .data$lithology_to_m[.data$lith_rec == .data$lith_n],
          finished_well_depth_ft_bgl = .data$lithology_to_ft_bgl[
            .data$lith_rec == .data$lith_n
          ],
          fix_depth_missing = TRUE,
          .by = "well_tag_number"
        )

      if (inherits(wells_sub, "sf")) {
        wells_sub <- dplyr::as_tibble(wells_sub) %>%
          dplyr::rows_upsert(w, by = c("well_tag_number", "lith_rec")) %>%
          sf::st_as_sf()
      } else {
        wells_sub <- dplyr::rows_upsert(
          wells_sub,
          w,
          by = c("well_tag_number", "lith_rec")
        )
      }
    } else {
      message(
        "Some wells are missing well depth. ",
        "Consider either using `fix_depth_missing = TRUE` in `wells_subset()` or ",
        "fixing the original record in GWELLS\n",
        "Wells: ",
        w_pretty
      )
    }
  }

  wells_sub
}


#' Fix the depth of the well if not equal to final lithology
#'
#' The `flag_depth_mismatch` flag identifies wells where the depth is not the
#' same as the depth of the final lithology layer. Fixing these well depths
#' means replacing the well depth with the depth of the final lithology layer.
#'
#' This is done automatically for leapfrog exports, but otherwise not.
#'
#' @inheritParams common_docs
#'
#' @return Fixed wells_sub data frame
#' @examplesIf interactive()
#'
#' library(sf)
#'
#' # Load a shape file defining the region of interest
#' creek_sf <- st_read("misc/data/Clinton_Creek.shp")
#'
#' # Get wells within this region
#' creek_wells <- wells_subset(creek_sf)
#'
#' # Fix well depths
#' creek_wells_fixed <- fix_depth_mismatch(creek_wells)
#'
#' # Explore all fixes
#' dplyr::select(creek_wells_fixed, dplyr::starts_with("fix"))
#'
#' @export
fix_depth_mismatch <- function(wells_sub) {
  if (!"fix_depth_mismatch" %in% names(wells_sub)) {
    wells_sub$fix_depth_mismatch <- FALSE
  }

  # Which wells are fixable and haven't been?
  w <- wells_sub$well_tag_number[
    wells_sub$flag_depth_mismatch &
      !wells_sub$flag_lith_missing
  ] |>
    unique()
  w_pretty <- paste0(w, collapse = ", ")

  message(
    "Fixing wells where depth is not equal to final lithology layer: ",
    w_pretty
  )

  w <- wells_sub %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(.data$well_tag_number %in% .env$w) %>%
    dplyr::mutate(
      well_depth_m = .data$lithology_to_m[.data$lith_rec == .data$lith_n],
      finished_well_depth_ft_bgl = .data$lithology_to_ft_bgl[
        .data$lith_rec == .data$lith_n
      ],
      fix_depth_mismatch = TRUE,
      .by = "well_tag_number"
    )

  if (inherits(wells_sub, "sf")) {
    wells_sub <- dplyr::as_tibble(wells_sub) %>%
      dplyr::rows_upsert(w, by = c("well_tag_number", "lith_rec")) %>%
      sf::st_as_sf()
  } else {
    wells_sub <- dplyr::rows_upsert(
      wells_sub,
      w,
      by = c("well_tag_number", "lith_rec")
    )
  }

  wells_sub
}


#' Fix the yield of a well if equal to zero
#'
#' The `flag_yield_zero` flag identifies wells where the yield was marked as 0
#' in GWELLS but probably should have been recorded as `NA`. Fixing these well
#' yields means replacing the yield of 0 with `NA`.
#'
#' @param wells_sub Data frame. The subsetted Wells data frame.
#' @param fix Logical. Whether to apply the fix.

fix_yield_zero <- function(wells_sub, fix = TRUE) {
  if (!"fix_yield_zero" %in% names(wells_sub)) {
    wells_sub$fix_yield_zero <- FALSE
  }

  # Which wells are fixable and haven't been?
  w <- wells_sub$well_tag_number[
    wells_sub$flag_yield_zero & !wells_sub$fix_yield_zero
  ] |>
    unique()
  w_pretty <- paste0(w, collapse = ", ")

  if (length(w) > 0) {
    if (fix) {
      message("Fixing wells where yield 0 should be NA: ", w_pretty)

      wells_sub <- wells_sub %>%
        dplyr::mutate(
          well_yield_usgpm = dplyr::na_if(.data$well_yield_usgpm, 0),
          fix_yield_zero = TRUE
        )
    } else {
      message(
        "Some wells have a yield of 0 which should probably be `NA`. ",
        "Consider either using `fix_yield_zero = TRUE` in `wells_subset()` or ",
        "fixing the original record in GWELLS\n",
        "Wells: ",
        w_pretty
      )
    }
  }

  wells_sub
}
