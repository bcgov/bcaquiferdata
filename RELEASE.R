

data_update()


# Update the app helpfiles ------------------
# - inst/extra_docs/lithology_desc.md

# Update NEWS --------------

# Update Datasets
source("data-raw/flags.R")
source("data-raw/tiles.R")
source("data-raw/internal.R")

# Compile README
unlink("README_files", recursive = TRUE)
devtools::build_readme()

# Preview website
pkgdown::build_site()
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("lithology_categorization")

# Preview app
aq_app()
