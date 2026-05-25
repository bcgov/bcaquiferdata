# Clean cache

Removes data cache

## Usage

``` r
cache_clean(bcmaps_cded = FALSE)
```

## Arguments

- bcmaps_cded:

  Logical. Whether or not to also remove CDED files cached with the
  bcmaps package. These are used by bcaquifertools for acquiring TRIM
  data, but may also be cached for use by other workflows.

## Examples

``` r

# cache_clean()
# cache_clean(bcmaps_cded = TRUE)
```
