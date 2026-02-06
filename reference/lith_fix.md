# Fix lithology descriptions

Clean and categorize lithology descriptions into primary, secondary,
tertiary and final lithology categories. Generally this function is used
internally when loading and cleaning GWELLS lithology.

## Usage

``` r
lith_fix(file = "lithology.csv", desc = NULL)
```

## Arguments

- file:

  Character. Lithology file name stored in cache

- desc:

  Character. Text string to convert (overrides `file`).

## Value

Data frame of lithology categorizations

## Details

However statements can be tested directly with this function to see how
it works and for troubleshooting.

## Examples

``` r
lith_fix(desc = "sandy gravel")
#>   lithology_raw_data lithology_clean lith_primary lith_secondary lith_tertiary
#> 1       sandy gravel    sandy gravel       gravel                         sand
#>   lithology_extra      lithology_category yield_units flag_bedrock
#> 1                 Sand and Gravel (Clean)                    FALSE
#>   flag_boulders flag_missing_cats
#> 1         FALSE             FALSE

# basic spell checks
lith_fix(desc = "saandy gravel")
#>   lithology_raw_data lithology_clean lith_primary lith_secondary lith_tertiary
#> 1      saandy gravel    sandy gravel       gravel                         sand
#>   lithology_extra      lithology_category yield_units flag_bedrock
#> 1                 Sand and Gravel (Clean)                    FALSE
#>   flag_boulders flag_missing_cats
#> 1         FALSE             FALSE
```
