# Roadmap: Workflow and Design Plan

This document compiles the ideas and their implementation discussed
around this project. Here we describe the various steps, list how they
are implemented in both the R code/functions as well as in the Shiny App
([`aq_app()`](https://bcgov.github.io/bcaquiferdata/reference/aq_app.md)).

Status and outstanding tasks/questions are listed below each item.

Here is a summary with links to the relevant sections

- [Downloading GWELLS](#downloading-gwells) - **Complete**
- [Standardize Lithology](#standardize-lithology) - **Polishing**
- [Defining area of interest](#defining-area-of-interest) - **Complete**
- [Fetching elevation data](#fetching-elevation-data) - **Complete**
- [Adding elevation data](#adding-elevation-data) - **Complete**
- [Adding spatial data layers](#adding-spatial-data-layers) - **Not
  implemented**
- [Water flow analysis](#water-flow-analysis) - **Partially complete**
- [Cross-sections](#cross-sections) - **Partially complete**
- [Flagging issues](#flagging-issues) - **Partially complete**
- [R package development](#r-package-development) - **Incomplete**

### Downloading GWELLS

- \[Code\] Functions check for GWELLS data in cache and download as
  needed. User can use
  [`data_update()`](https://bcgov.github.io/bcaquiferdata/reference/data_update.md)
  to download/update local cache. User can load data directly using
  [`data_read()`](https://bcgov.github.io/bcaquiferdata/reference/data_read.md)
  (but not necessary as functions will do this automatically).

- \[App\] First tab offers information on state of data in cache and
  options to download or clear cache

> **Status** - Complete

### Standardize Lithology

- As part of the GWELLS data download, lithology statements are
  processed into simplified categories
- The steps of this process result in several new columns added
  - Spelling mistakes are corrected, multiple terms are collapsed
    (`lithology_clean`)
  - Remaining terms are established as primary, secondary, or tertiary
    (`lith_primary`, `lith_secondary`, `lith_tertiary`
  - These are then categorized into `lithology_category`
  - Special terms are listed in `lithology_extra` (e.g., “wet”)

> **Status** - Complete (and forever polishing)

### Defining area of interest

- \[Code\] User loads a shape file via the `sf` package
  ([`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html))
- \[App\] User selects shape files or zip with shape files

> **Status** - Complete

### Fetching elevation data

**Sources** - Lidar -
<https://catalogue.data.gov.bc.ca/dataset/lidarbc-open-lidar-data-portal> -
TRIM DEM -
~~<https://catalogue.data.gov.bc.ca/dataset/1-20-000-georeferenced-topographic-base-maps-by-mapsheet>~~ -
<https://catalogue.data.gov.bc.ca/dataset/digital-elevation-model-for-british-columbia-cded-1-250-000> -
Custom DEM

**Steps** - Using area of interest, download relevant DEM data to the
cache, load it, and trim it to the area of interest. - \[Code\] User
passes area of interest to
[`dem_region()`](https://bcgov.github.io/bcaquiferdata/reference/dem_region.md)
function - \[App\] Automatically happens once area of interest is
selected

### Adding elevation data

- Using region of interest, subset full GWELLS data to wells of interest
- Join spatially with elevation data
- Option to use combination of Lidar and TRIM
- \[Code\] Subset with
  [`wells_subset()`](https://bcgov.github.io/bcaquiferdata/reference/wells_subset.md),
  add elevation with
  [`wells_elev()`](https://bcgov.github.io/bcaquiferdata/reference/wells_elev.md)
  - `dem` is the primary source, `dem_extra` is the optional secondary
    source
- \[App\] Automatically happens once area of interest is selected
  - toggle whether to use Lidar, TRIM, Lidar with Trim, or TRIM with
    Lidar

> **Status** - Complete

### Adding spatial data layers

> Spatial data trimmed to match the extent of the area of interest.

- Bedrock geology:
  <https://catalogue.data.gov.bc.ca/dataset/bedrock-geology>
- Faults: <https://catalogue.data.gov.bc.ca/dataset/geology-faults>
- Quaternary Mapping:
  <https://catalogue.data.gov.bc.ca/dataset/geology-quaternary-alluvium-and-cover>
- Aquifers:
  <https://catalogue.data.gov.bc.ca/dataset/ground-water-aquifers>
- Freshwater Atlas lakes:
  <https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-lakes>
- Freshwater Atlas wetlands:
  <https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-wetlands>
- Freshwater Atlas streams:
  <https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network>

**Steps**

- Add option to download spatial data layers (or as needed?)
- Add option to export cropped spatial data

> **Status** - In Progress (on hold)
>
> **TODO** - [Add Freshwater
> atlas](https://github.com/bcgov/bcaquiferdata/issues/26) - [Potential
> others](https://github.com/bcgov/bcaquiferdata/issues/19)

### Water flow/yield

- Extract water flow/yield details from lithology descriptions
- Adds four columns: yield, yield units, depth, and depth units
- Where there are multiple yields per record, corresponding to flow at
  multiple depths, extra records are created (currently duplicated)
- Add a flow column with a flag (?) - Where ‘fracture’, any ‘wet’ term
  (e.g., ‘flow’, ‘seepage’, etc.), or ‘aquifer’, in lithology
  description \[**Not implemented**\]
- \[Code\] User formats their data for water flow analysis using the
  [`wells_yield()`](https://bcgov.github.io/bcaquiferdata/reference/wells_yield.md)
  function
- \[App\] The hydrostratigraphy tab shows the data formated for water
  flow analysis.

> **Status** - Complete

### Cross-sections

- Export data in formats useful for Cross-section analyses
- Strater
- Voxler
- ArcHydro
- Leapfrog
- \[Code\] Users can export the data to various formats with
  [`wells_export()`](https://bcgov.github.io/bcaquiferdata/reference/wells_export.md)
- \[App\] The Exports tab previews the data in different formats with
  the option to save/download zipped copies of the files.

> **Status** - Complete

### Flagging issues

- Common problems with yield or lithology can be flagged (flag columns
  added)
- \[Code\] `flag_XXX` columns are added to the data outputs. The `flags`
  data set includes a glossary explaining the flags
- \[App\] `flag_XXX` columns are added to the data outputs. There is a
  dedicated ‘flags’ tab for exploring flags which includes a copy of the
  glossary in the `flags` data set.

> **Status** - Complete

### R package development

- Adding checks (give users informative feedback if they use a function
  incorrectly)
- Adding tests (ensure that lithology categorizations are consistent,
  and that functions work as expected)
- Adding documentation (help users use the app and/or functions)
  - Function documentation
  - Internal documentation
  - pkgdown website

> **Status** - In Progress
>
> **TODO**
>
> - All of the above!
