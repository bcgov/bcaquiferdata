# Download, Update, and/or load data

This function downloads, updates or loads locally stored data. Currently
this function returns `wells`, `wells_sf`, or `lithology` data. Note
that these data are originally from GWELLS, but are cleaned and
summarized for use in the bcaquiferdata package. For example `wells_sf`
is a spatial version of the data, and `lithology` is a cleaned and
standardized version of lithology. `wells` also contains the new
standardized `lithology` data, along with the original lithology
observations and intermediate classification steps to simplify error
tracing.

## Usage

``` r
data_read(type, update = FALSE, permission = FALSE)
```

## Arguments

- type:

  Character. Type of data to return, one of `wells`, `wells_sf`, or
  `lithology`

- update:

  Logical. Force update of the data?

- permission:

  Logical. Permission to create the cache folder. If `FALSE`, user is
  asked for permission, if `TRUE`, permission is implied.

## Value

Data frame or spatial features object of the requested data.

## Details

Under normal circumstances, users will not need to use this function as
it is used internally by the main workflow functions. However, users may
wish to overview entire datasets.

Bear in mind that the lithology cleaning and standardizing, while better
than the original data, will almost certainly still have errors!

## Examples

``` r
if (FALSE) { # interactive()
wells <- data_read("wells")
}
```
