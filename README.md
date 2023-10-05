
# bcaquiferdata

This is an R package for processing BC Gov GWELLS data into formats more
suitable for other analyses (e.g., Strater, Voxler, ArcHydro).

The functions provided here allow users to filter GWELLS data by region
(shapefile), calculate and add elevation data from Lidar or TRIM, and
clean and categorize GWELLS lithology and yield data.

The Shiny app (a graphical user interface), provides a tool for data
exploration and exporting.

This is work is still in the preliminary stages, but is testable.

To install:

``` r
install.packages("bcaquiferdata", 
                 repos = c(bcaquiferdata = "https://steffilazerte.r-universe.dev", 
                           getOption("repos")))
```

## General Workflow

### Shiny App

To run the Shiny app

``` r
library(bcaquiferdata)
aq_app()
```

### By Code

A general workflow ‘by hand’ (without using the app) is to run through
the various steps using the functions directly.

This gives you a bit more flexibility in how you explore and/or filter
your data.

Let’s work through a couple of examples.

``` r
library(bcaquiferdata)
library(sf)
library(ggplot2)
library(ggspatial)
```

#### Clinton Creek

Load a shape file defining the region of interest

``` r
creek_sf <- st_read("misc/data/Clinton_Creek.shp")
```

    ## Reading layer `Clinton_Creek' from data source 
    ##   `/home/steffi/Projects/Business/BC Government/bcaquiferdata/misc/data/Clinton_Creek.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 15 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 1294896 ymin: 671234.7 xmax: 1315854 ymax: 695645.2
    ## Projected CRS: NAD83 / BC Albers

Fetch Lidar DEM (this may take a while the first time)

``` r
creek_lidar <- dem_region(creek_sf)
```

    ## Get Lidar data

    ## Saving new tiles to cache directory: ~/.local/share/bcaquiferdata

    ## Checking for matching tifs

    ## Fetching bc_092p002_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p013_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p012_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092i092_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p003_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Cropping DEM to region

Plot to double check

``` r
plot(creek_lidar)
```

    ## downsample set to 39

![](man/figures/unnamed-chunk-7-1.png)<!-- -->

Collect wells in this region with added elevation from Lidar

``` r
creek_wells <- creek_sf |>
  wells_subset() |>        # Subset to region
  wells_elev(creek_lidar)  # Add Lidar
```

    ## Subset wells

    ## Add elevation

Plot again to double check

``` r
ggplot() +
  geom_sf(data = creek_sf) +
  geom_sf(data = creek_wells, size= 1, aes(colour = elev))
```

![](man/figures/unnamed-chunk-9-1.png)<!-- -->

Export data for Strater, Voxler, and ArcHydro

``` r
wells_export(creek_wells, id = "clinton", type = "strater")
```

    ## Writing Strater files ./clinton_strater_lith.csv, ./clinton_strater_collars.csv, ./clinton_strater_wls.csv

    ## [1] "./clinton_strater_lith.csv"    "./clinton_strater_collars.csv"
    ## [3] "./clinton_strater_wls.csv"

``` r
wells_export(creek_wells, id = "clinton", type = "voxler")
```

    ## Writing Voxler file ./clinton_voxler.csv

    ## [1] "./clinton_voxler.csv"

``` r
wells_export(creek_wells, id = "clinton", type = "archydro")
```

    ## Writing ArcHydro files ./clinton_archydro_well.csv, ./clinton_archydro_hguid.csv, ./clinton_archydro_bh.csv

    ## [1] "./clinton_archydro_well.csv"  "./clinton_archydro_hguid.csv"
    ## [3] "./clinton_archydro_bh.csv"

#### Mill Bay Watershed

Load a shape file defining the region of interest

``` r
mill_sf <- st_read("misc/data/MillBayWatershed.shp")
```

    ## Reading layer `MillBayWatershed' from data source 
    ##   `/home/steffi/Projects/Business/BC Government/bcaquiferdata/misc/data/MillBayWatershed.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 1 feature and 16 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 1175893 ymin: 402094.9 xmax: 1181462 ymax: 407633.1
    ## Projected CRS: NAD83 / BC Albers

We’ll check against some tiles

``` r
g <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = -1) +
  geom_sf(data = mill_sf, fill = NA, linewidth = 1.5) +
  labs(caption = "Data from OpenStreet Map")
g
```

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

Fetch Lidar DEM (this may take a while the first time)

``` r
mill_lidar <- dem_region(mill_sf)
```

    ## Get Lidar data

    ## Saving new tiles to cache directory: ~/.local/share/bcaquiferdata

    ## Checking for matching tifs

    ## Fetching bc_092b063_xl1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092b062_xl1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Cropping DEM to region

Add to our plot to double check

``` r
mill_lidar_sf <- stars::st_downsample(mill_lidar, n = 12) |> # Downsample first
  st_as_sf(as_points = FALSE, merge = TRUE)         # Convert to polygons
```

    ## for stars_proxy objects, downsampling only happens for dimensions x and y

``` r
g + geom_sf(data = mill_lidar_sf, aes(fill = elev), colour = NA)
```

    ## Zoom: 13

![](man/figures/unnamed-chunk-14-1.png)<!-- -->

Looks like we don’t have elevation data for the whole region. This can
be confirmed by checking the online [LidarBC
map](https://governmentofbc.maps.arcgis.com/apps/MapSeries/index.html?appid=d06b37979b0c4709b7fcf2a1ed458e03)

Let’s take a look our our options using TRIM data.

``` r
mill_trim <- dem_region(mill_sf, type = "trim")
```

    ## Get TRIM data

    ## checking your existing tiles for mapsheet 92b are up to date

    ## Cropping DEM to region

Add to our plot to double check

``` r
mill_trim_sf <- mill_trim |>
  st_as_sf(as_points = FALSE, merge = TRUE)         # Convert to polygons

g + geom_sf(data = mill_trim_sf, aes(fill = elev), colour = NA)
```

    ## Zoom: 13

![](man/figures/unnamed-chunk-16-1.png)<!-- -->

TRIM is at a coarser resolution, but covers our entire area. Let’s use
it instead.

Collect wells in this region with added elevation from TRIM.

``` r
mill_wells <- mill_sf |>
  wells_subset() |>
  wells_elev(mill_trim)
```

    ## Subset wells

    ## Add elevation

Plot again to double check, see that we now elevation data for all
wells.

``` r
g +
  geom_sf(data = mill_wells, size = 1, aes(colour = elev)) +
  scale_color_viridis_c(na.value = "red")
```

    ## Zoom: 13

![](man/figures/unnamed-chunk-18-1.png)<!-- -->

Export data for Strater, Voxler, and ArcHydro

``` r
wells_export(mill_wells, id = "mill", type = "strater")
```

    ## Writing Strater files ./mill_strater_lith.csv, ./mill_strater_collars.csv, ./mill_strater_wls.csv

    ## [1] "./mill_strater_lith.csv"    "./mill_strater_collars.csv"
    ## [3] "./mill_strater_wls.csv"

``` r
wells_export(mill_wells, id = "mill", type = "voxler")
```

    ## Writing Voxler file ./mill_voxler.csv

    ## [1] "./mill_voxler.csv"

``` r
wells_export(mill_wells, id = "mill", type = "archydro")
```

    ## Writing ArcHydro files ./mill_archydro_well.csv, ./mill_archydro_hguid.csv, ./mill_archydro_bh.csv

    ## [1] "./mill_archydro_well.csv"  "./mill_archydro_hguid.csv"
    ## [3] "./mill_archydro_bh.csv"

### Extra tools

``` r
library(dplyr)
library(readr)
```

Load cleaned data (will fetch if doesn’t already exist)

``` r
wells_lith <- data_read("lithology")
```

Explore the lithology standardization performed by bcaquiferdata

``` r
lith_std <- wells_lith |>
  select(well_tag_number, contains("lith")) |>
  arrange(!is.na(lithology_category))
lith_std
```

    ## # A tibble: 614,295 × 17
    ##    well_tag_number lithology_from_ft_bgl lithology_to_ft_bgl lithology_raw_data 
    ##              <dbl>                 <dbl>               <dbl> <chr>              
    ##  1              11                   164                 187 "red ash"          
    ##  2              13                     1                 120 "\""               
    ##  3              49                     0                  15  <NA>              
    ##  4              62                     0                   0 "backfilled to 217…
    ##  5              73                    25                 170 "delimite w/copper…
    ##  6              73                   190                 380 "copper ore w/deli…
    ##  7              88                     0                  65  <NA>              
    ##  8              98                     0                  15  <NA>              
    ##  9             105                     0                  15  <NA>              
    ## 10             163                   200                 210 "gray,clean a litt…
    ## # ℹ 614,285 more rows
    ## # ℹ 13 more variables: lithology_description_code <chr>,
    ## #   lithology_material_code <chr>, lithology_hardness_code <chr>,
    ## #   lithology_colour_code <chr>, lithology_observation <chr>,
    ## #   lithology_from_m <dbl>, lithology_to_m <dbl>, lithology_clean <chr>,
    ## #   lith_primary <chr>, lith_secondary <chr>, lith_tertiary <chr>,
    ## #   lithology_extra <chr>, lithology_category <chr>

Save it to peruse later

``` r
write_csv(lith_std, "lith_categorization.csv")
```
