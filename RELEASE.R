# Copyright 2024 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# Check version -------------------------------

# Update the app helpfiles -----------------------
# - inst/extra_docs/lithology_desc.md

# Update NEWS ------------------------------------
file.edit("NEWS.md")

# Update WORKFLOW AND DESIGN
file.edit("WORKFLOW AND DESIGN.md")

# Update CODE_DESIGN
file.edit("CODE_DESIGN.md")

# Update Datasets --------------------------------
data_update()
source("data-raw/flags.R")
source("data-raw/tiles.R")
source("data-raw/internal.R")
source("data-raw/test_data.R")

# Update citation information!
citation("bcaquiferdata")

# Compile README ---------------------------------
unlink("README_files", recursive = TRUE)
devtools::build_readme()

# Precompile vignettes
source("vignettes/_precompile.R")

# Run tests - Compare snapshots - BUILD PACKAGE FIRST ------------------
#devtools::test()
#testthat::snapshot_review()
#testthat::snapshot_review(path = "inst/shiny/tests/testthat")

# Release --------------------------------------------------------------

## Push to GitHub!
## Going up a version? Create signed release on github
usethis::use_github_release()


# Preview website - "Real" version run as GitHub action -----------------
# **BUILD PACKAGE FIRST!!!**
pkgdown::build_site()
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("articles/shiny_apps")
pkgdown::build_article("bcaquiferdata")
pkgdown::build_article("flags")

# Preview app -----------------------------------------------------------
aq_app()
