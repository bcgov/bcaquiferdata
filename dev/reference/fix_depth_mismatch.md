# Fix the depth of the well if not equal to final lithology

The `flag_depth_mismatch` flag identifies wells where the depth is not
the same as the depth of the final lithology layer. Fixing these well
depths means replacing the well depth with the depth of the final
lithology layer.

## Usage

``` r
fix_depth_mismatch(wells_sub)
```

## Arguments

- wells_sub:

  sf spatial data frame. Subset of wells data output by
  [`wells_subset()`](https://bcgov.github.io/bcaquiferdata/dev/reference/wells_subset.md)

## Value

Fixed wells_sub data frame

## Details

This is done automatically for leapfrog exports, but otherwise not.

## Examples

``` r
if (FALSE) { # interactive()

library(sf)

# Load a shape file defining the region of interest
creek_sf <- st_read("misc/data/Clinton_Creek.shp")

# Get wells within this region
creek_wells <- wells_subset(creek_sf)

# Fix well depths
creek_wells_fixed <- fix_depth_mismatch(creek_wells)

# Explore all fixes
dplyr::select(creek_wells_fixed, dplyr::starts_with("fix"))
}
```
