# Mini GWELLs Wells ----------------------------------------------
# For testing wells_clean()

# Get the data
f <- file.path(cache_dir(), "GWELLS/well.csv")
if(!file.exists(f)) fetch_gwells()

# Bundle subset of data
writeLines(
  readLines(f, n = 100),
  file.path("inst", "extdata", "test_gwells_wells.csv")
)

# Mini GWELLs lithology ----------------------------------------------
# For testing lith_prep()

# Get the data
f <- file.path(cache_dir(), "GWELLS/lithology.csv")
if(!file.exists(f)) fetch_gwells()

# Bundle subset of data
writeLines(
  readLines(f, n = 3000)[c(1, 2933:3000)],
  file.path("inst", "extdata", "test_gwells_lithology.csv")
)

# Expected lithology categories --------------------------------------------
#  For testing lith_fix()
readr::write_csv(
  dplyr::tribble(
    ~desc, ~cat,
    "brown clay","Clay",
    "brown sand and gravel, silty","Sand and Gravel (Dirty)",
    "brown silt & gravel","Gravel (Dirty)",
    "clay","Clay",
    "clay/silty","Clay",
    "gravel (cemented)","Sand or Gravel Till or Diamicton",
    "gravel/silty","Gravel (Dirty)",
    "grey & some quartz, bedrock","Bedrock",
    "grey clay","Clay",
    "hard earth","Medium to Clay Till or Diamicton",
    "pea gravel","Gravel",
    "silty clay","Clay",
    "silty sand and gravel layers","Sand and Gravel (Dirty)",
    "silty sticky clay","Clay",
    "soft/medium; clay","Clay",
    "very hard clay","Clay",
    "water bearing sand gravel layer of clay","Sand and Gravel (Dirty)",
    "dense gray silt, sand","Sand and Fines",
    "silt & sand fine-medium","Sand and Fines",
    "silt, sand fine-medium","Sand and Fines",
    "sandy till","Sand or Gravel Till or Diamicton",
    "rocks", "Gravel",
    "rock", "Bedrock",
    "solid rock", "Bedrock",
    "soft rock", "Bedrock",
    "fractured red rock", "Weathered, Fractured or Faulted Bedrock",
    "broken rock", "Weathered, Fractured or Faulted Bedrock",
    "shattered rock", "Weathered, Fractured or Faulted Bedrock",
    "hard packed gravel & boulders", "Sand or Gravel Till or Diamicton",
    "shells", "Shells",
    "shells and sand", "Sand",
    "shells and peat", "Organics",
    "cemented sand", "Sand or Gravel Till or Diamicton",
    "hardpacked with sand", "Sand or Gravel Till or Diamicton",
    "silt & sand with gravel", "Sand and Gravel (Dirty)",
    "tilly silt", "Medium to Clay Till or Diamicton",
    "silt till", "Medium to Clay Till or Diamicton",
    "tilly clay", "Medium to Clay Till or Diamicton",
    "silt hard packed", "Medium to Clay Till or Diamicton",
    "sand & granite", "Bedrock",
    "sandy", "Sand",
    "silty", "Silt",
    "clayey", "Clay",
    "clay and silt", "Medium to Clay Till or Diamicton",
    "silt silty clay", "Medium to Clay Till or Diamicton",
    "med. to hard granite", "Bedrock",
    "aquifer data: glacial till", "Medium to Clay Till or Diamicton",
    "dirty gravel with some sandy seams", "Sand and Gravel (Dirty)",
    "gravelly sand", "Sand and Gravel (Clean)",
    "gravelly sand with silt", "Sand and Gravel (Dirty)",
    "sandy gravel", "Sand and Gravel (Clean)",
    "sandy gravel with silt", "Sand and Gravel (Dirty)",
    "silty gravel with sand", "Sand and Gravel (Dirty)",
    "gravely silt with sand", "Sandy or Gravelly Silt",
    "gravel & sand & clay", "Sand and Gravel (Dirty)",
    "silt with till", "Medium to Clay Till or Diamicton",
    "clay with till", "Medium to Clay Till or Diamicton",
    "silt clay with till", "Medium to Clay Till or Diamicton",
    "blue hardpan, sand and broken gravel", "Medium to Clay Till or Diamicton",
    # Tests for combined columns (lithology_raw_data, lithology_description_code, lithology_material_code)
    "gravel  gravel", "Gravel",
    "sand and silt silty sand", "Sand and Fines",
  ),
  file.path("inst", "extdata", "test_lithology_cleaning.csv"))

