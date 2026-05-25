# Fix the yield of a well if equal to zero

The `flag_yield_zero` flag identifies wells where the yield was marked
as 0 in GWELLS but probably should have been recorded as `NA`. Fixing
these well yields means replacing the yield of 0 with `NA`.

## Usage

``` r
fix_yield_zero(wells_sub, fix = TRUE)
```

## Arguments

- wells_sub:

  Data frame. The subsetted Wells data frame.

- fix:

  Logical. Whether to apply the fix.
