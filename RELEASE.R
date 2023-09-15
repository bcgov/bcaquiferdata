
# Compile README
unlink("README_files", recursive = TRUE)
devtools::build_readme()


data_update()


# Update the app helpfiles ------------------
# - inst/extra_docs/lithology_desc.md

# Update NEWS --------------




# Preview website
pkgdown::build_site()
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("lithology_categorization")
