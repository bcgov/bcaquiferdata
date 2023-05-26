
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
                 repos = c("https://steffilazerte.r-universe.dev", 
                           "https://cloud.r-project.org"))
```

## General Workflow

### Shiny App

To run the Shiny app

``` r
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

    ## Saving tiles to cache directory: ~/.local/share/bcaquiferdata

    ## Checking for matching tifs

    ## Fetching bc_092i092_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p002_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p003_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p012_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092p013_xli1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Cropping DEM to region

Plot to double check

``` r
plot(creek_lidar)
```

    ## downsample set to 39

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Export data for Strater, Voxler, and ArcHydro

``` r
wells_export(creek_wells, id = "clinton", type = "strater")
```

    ## Writing Strater files ./clinton_lith.csv, ./clinton_collars.csv, ./clinton_wls.csv

    ## [1] "./clinton_lith.csv"    "./clinton_collars.csv" "./clinton_wls.csv"

``` r
wells_export(creek_wells, id = "clinton", type = "voxler")
```

    ## Writing Voxler file ./clinton_vox

    ## [1] "./clinton_vox"

``` r
wells_export(creek_wells, id = "clinton", type = "archydro")
```

    ## Writing ArcHydro files ./clinton_arc_well.csv, ./clinton_arc_hguid.csv, ./clinton_arc_bh.csv

    ## [1] "./clinton_arc_well.csv"  "./clinton_arc_hguid.csv"
    ## [3] "./clinton_arc_bh.csv"

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

    ## Zoom: 13

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Fetch Lidar DEM (this may take a while the first time)

``` r
mill_lidar <- dem_region(mill_sf)
```

    ## Get Lidar data

    ## Saving tiles to cache directory: ~/.local/share/bcaquiferdata

    ## Checking for matching tifs

    ## Fetching bc_092b062_xl1m_utm10_2019.tif - skipping (new_only = TRUE)

    ## Fetching bc_092b063_xl1m_utm10_2019.tif - skipping (new_only = TRUE)

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

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Export data for Strater, Voxler, and ArcHydro

``` r
wells_export(mill_wells, id = "mill", type = "strater")
```

    ## Writing Strater files ./mill_lith.csv, ./mill_collars.csv, ./mill_wls.csv

    ## [1] "./mill_lith.csv"    "./mill_collars.csv" "./mill_wls.csv"

``` r
wells_export(mill_wells, id = "mill", type = "voxler")
```

    ## Writing Voxler file ./mill_vox

    ## [1] "./mill_vox"

``` r
wells_export(mill_wells, id = "mill", type = "archydro")
```

    ## Writing ArcHydro files ./mill_arc_well.csv, ./mill_arc_hguid.csv, ./mill_arc_bh.csv

    ## [1] "./mill_arc_well.csv"  "./mill_arc_hguid.csv" "./mill_arc_bh.csv"

### Extra tools

``` r
library(dplyr)
library(readr)
```

Load cleaned data (will fetch if doesn’t already exist)

``` r
wells_lith <- data_read("lithology", update = TRUE)
```

    ## Downloading GWELLS data

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

    ## Wells - Cleaning

    ## Wells - Saving data to cache

    ## Lithology - Cleaning

    ## Lithology - Standardizing

    ## Lithology - Saving data to cache

Explore the lithology standardization performed by bcaquiferdata

``` r
lith_std <- wells_lith |>
  select(well_tag_number, contains("lith")) |>
  arrange(!is.na(lithology_category))
lith_std
```

    ## # A tibble: 612,373 × 17
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
    ## # ℹ 612,363 more rows
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
