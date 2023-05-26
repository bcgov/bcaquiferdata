
# Compile README
unlink("README_files", recursive = TRUE)
devtools::build_readme()
