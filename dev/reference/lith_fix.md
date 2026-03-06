# Fix lithology descriptions

Clean and categorize lithology descriptions into primary, secondary,
tertiary and final lithology categories. Generally this function is used
internally when loading and cleaning GWELLS lithology.

## Usage

``` r
lith_fix(desc = NULL)
```

## Arguments

- desc:

  Character. Text string to convert/fix.

## Value

Data frame of lithology categorizations

## Details

However statements can be tested directly with this function to see how
it works and for troubleshooting.

## Examples

``` r
lith_fix("sandy gravel")
#> # A tibble: 1 × 11
#>   lithology_raw_combined lithology_clean lith_primary lith_secondary
#>   <chr>                  <chr>           <chr>        <chr>         
#> 1 sandy gravel           sandy gravel    gravel       ""            
#> # ℹ 7 more variables: lith_tertiary <chr>, lithology_extra <chr>,
#> #   lithology_category <chr>, yield_units <chr>, flag_cat_bedrock <lgl>,
#> #   flag_cat_boulders <lgl>, flag_cat_missing <lgl>

# basic spell checks
lith_fix("saandy gravel")
#> # A tibble: 1 × 11
#>   lithology_raw_combined lithology_clean lith_primary lith_secondary
#>   <chr>                  <chr>           <chr>        <chr>         
#> 1 saandy gravel          sandy gravel    gravel       ""            
#> # ℹ 7 more variables: lith_tertiary <chr>, lithology_extra <chr>,
#> #   lithology_category <chr>, yield_units <chr>, flag_cat_bedrock <lgl>,
#> #   flag_cat_boulders <lgl>, flag_cat_missing <lgl>
```
