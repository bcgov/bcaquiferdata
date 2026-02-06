# Subset wells to region

Filter the GWELLS data returning only wells within the provided
shapefile.

## Usage

``` r
wells_subset(region, update = FALSE)
```

## Arguments

- region:

  sf simple features object. Shape file of the region of interest.

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
