# Add yield lithology data to wells subset

Yield records are extracted from lithology observations and added to the
wells data.

## Usage

``` r
wells_yield(wells_sub)
```

## Arguments

- wells_sub:

  sf spatial data frame. Subset of wells data output by
  [`wells_subset()`](https://bcgov.github.io/bcaquiferdata/dev/reference/wells_subset.md)

## Value

Data frame or sf spatial data frame with wells data and added yield from
lithology.

## Examples

``` r
if (FALSE) { # interactive()

library(sf)

# Load a shape file defining the region of interest
creek_sf <- st_read("misc/data/Clinton_Creek.shp")

# Get wells within this region
creek_wells <- wells_subset(creek_sf)

# Get yield data for these wells
creek_yield <- wells_yield(creek_wells)
}
```
