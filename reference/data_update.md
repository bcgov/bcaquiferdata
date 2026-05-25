# Update cached data

Update the GWELLs data stored locally.

## Usage

``` r
data_update(
  type = c("wells", "lithology"),
  download = TRUE,
  permission = FALSE
)
```

## Arguments

- type:

  Character. Type of data to update. One of "all", "wells", "lithology"

- download:

  Logical. Whether to re-download and process the data (`TRUE`), or just
  re-process it (`FALSE`).

- permission:

  Logical. Permission to create the cache folder. If `FALSE`, user is
  asked for permission, if `TRUE`, permission is implied.

## Examples

``` r
if (FALSE) { # interactive()
data_update(type = "wells")
data_update(type = "lithology")
}
```
