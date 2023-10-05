# Workflow and Design Plan

This document compiles the ideas and their implementation discussed around this
project. Here we describe the various steps, list how they are implemented in
both the R code/functions as well as in the Shiny App (`aq_app()`). 

Status and outstanding tasks/questions are listed below each item.

Here is a summary with links to the relevant sections

- [Downloading GWELLS](#downloading-gwells) - **Complete**
- [Standardize Lithology](#standardize-lithology) - **Polishing**
- [Defining area of interest](#defining-area-of-interest) - **Complete**
- [Fetching elevation data](#fetching-elevation-data) - **Partially complete**
- [Adding elevation data](#adding-elevation-data) - **Complete**
- [Adding spatial data layers](#adding-spatial-data-layers) - **Not implemented**
- [Water flow analysis](#water-flow-analysis) - **Partially complete**
- [Cross-sections](#cross-sections) - **Partially complete**
- [Flagging issues](#flagging-issues) - **Partially complete**
- [R package development](#r-package-development) - **Incomplete**

### Downloading GWELLS
- [Code] Functions check for GWELLS data in cache and download as needed. 
  User can use `data_update()` to download/update local cache. User can load
  data directly using `data_read()` (but not necessary as functions will do this
  automatically).

- [App] First tab offers information on state of data in cache and options to
  download or clear cache
  
> **Status** - Complete

### Standardize Lithology
- As part of the GWELLS data download, lithology statements are processed into 
  simplified categories
- The steps of this process result in several new columns added
  - Spelling mistakes are corrected, multiple terms are collapsed (`lithology_clean`)
  - Remaining terms are established as primary, secondary, or tertiary (`lith_primary`, `lith_secondary`, `lith_tertiary`
  - These are then categorized into `lithology_category`
  - Special terms are listed in `lithology_extra` (e.g., "wet")

> **Status** - Polishing
> 
> **TODO**
> 
> - More questions
> - More refinement
> - What do we do with the 'lithology_extra' column?

### Defining area of interest
- [Code] User loads a shape file via the `sf` package (`sf::st_read()`)
- [App] User selects shape files or zip with shape files

> **Status** - Complete

### Fetching elevation data

**Sources**
- Lidar -  https://catalogue.data.gov.bc.ca/dataset/lidarbc-open-lidar-data-portal
- TRIM DEM 
    - ~~https://catalogue.data.gov.bc.ca/dataset/1-20-000-georeferenced-topographic-base-maps-by-mapsheet~~
    - https://catalogue.data.gov.bc.ca/dataset/digital-elevation-model-for-british-columbia-cded-1-250-000
  
**Steps**
- Using area of interest, download relevant DEM data to the cache, 
  load it, and trim it to the area of interest.
- [Code] User passes area of interest to `dem_region()` function
- [App] Automatically happens once area of interest is selected

> **Status** - Mostly complete
>
> **TODO**
> 
> - Add option for user to supply their own spatial elevation data

### Adding elevation data

- Using region of interest, subset full GWELLS data to wells of interest
- Join spatially with elevation data
- [Code] Subset with `wells_subset()`, add elevation with `wells_elev()`
- [App] Automatically happens once area of interest is selected

> **Status** - Complete

### Adding spatial data layers

> Spatial data trimmed to match the extent of the area of interst. 

- Bedrock geology: https://catalogue.data.gov.bc.ca/dataset/bedrock-geology
- Faults:  https://catalogue.data.gov.bc.ca/dataset/geology-faults
- Quaternary Mapping: https://catalogue.data.gov.bc.ca/dataset/geology-quaternary-alluvium-and-cover
- Aquifers: https://catalogue.data.gov.bc.ca/dataset/ground-water-aquifers
- Freshwater Atlas lakes: https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-lakes
- Freshwater Atlas wetlands: https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-wetlands
- Freshwater Atlas streams: https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network

> **Status** - Not implemented
>
> **TODO**
> 
> - Is this necessary or desired?
> - If so, what should it look like in the App?

### Water flow analysis
- Extract water flow details from lithology descriptions
- Adds four columns: yield, yield units, depth, and depth units
- Where there are multiple yields per record, corresponding to flow at multiple depths, extra records are created (currently duplicated)
- Add a flow column with a flag (?) - Where 'fracture', any 'wet' term (e.g., 'flow', 'seepage', etc.),
  or 'aquifer', in lithology description [**Not implemented**]
- [Code] User formats their data for water flow analysis using the `wells_yield()` function
- [App] The hydrostratigraphy tab shows the data formated for water flow analysis.

> **Status** - Partially complete
>
> **TODO**
> 
> - What next? Should this data be exportable from the app?
> - How do we create multiple records? Do we need to adjust the record depths?
> - Should the flow column be added?


### Cross-sections
 - Export data in formats useful for Cross-section analyses
 - Strater
 - Voxler
 - ArcHydro
 - [Code] Users can export the data to various formats with `wells_export()`
 - [App] The Exports tab previews the data in different formats with the option
   to save/download zipped copies of the files.

> **Status** - Partially complete
>
> **TODO** 
> 
> - [App] Should these be downloaded as a zip file or should the user be able to
>   state a diretory and have them saved there?
> - Test output files to ensure correct exports

### Flagging issues

- Common problems with yield or lithology can be flagged (flag columns added)
- [Code] `flag_XXX` columns are added to the data outputs. The `flags` data set
  includes a glossary explaining the flags
- [App] `flag_XXX` columns are added to the data outputs. There is a dedicated
  'flags' tab for exploring flags which includes a copy of the glossary in the
  `flags` data set.

> **Status** - Partially complete
>
> **TODO**
> 
> - Review flag specifics to ensure correct
> - Add more flags
> - What next? Option to export flags in App? 
>   (Note that all datasets in the App can be exported already, only relevant
>    if a special format is required)


### R package development
- Adding checks (give users informative feedback if they use a function incorrectly)
- Adding tests (ensure that lithology categorizations are consistent, and that 
functions work as expected)
- Adding documentation (help users use the app and/or functions)
  - Function documentation
  - pkgdown website
- Move package repository to `bcgov` on GitHub

> **Status** - Incomplete
> 
> **TODO**
> 
> - All of the above!
