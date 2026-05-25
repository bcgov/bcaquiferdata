# Subset wells to region

Filter the GWELLS data returning only wells within the provided
shapefile.

## Usage

``` r
wells_subset(
  region,
  fix_bottom_intervals = TRUE,
  fix_depth_missing = TRUE,
  fix_yield_zero = TRUE,
  update = FALSE
)
```

## Arguments

- region:

  sf simple features object. Shape file of the region of interest.

- fix_bottom_intervals:

  Logical. Whether to add 1m to bottom lithology intervals that has no
  thickness (identified by `flat_int_bottom`). Default `TRUE`.

- fix_depth_missing:

  Logical. Whether to fix missing well depths by making them equal to
  the depth of the final lithology layer. Default `TRUE`.

- fix_yield_zero:

  Logical. Whether to fix well yields of 0 by making them `NA`. Default
  `TRUE`.

- update:

  Logical. Force update of the data?

## Examples

``` r
if (FALSE) { # interactive()

library(sf)

# Load a shape file defining the region of interest
creek_sf <- st_read("misc/data/Clinton_Creek.shp")

# Get wells within this region
creek_wells <- wells_subset(creek_sf)
}
```
