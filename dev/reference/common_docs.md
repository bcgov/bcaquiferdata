# Common arguments and documentation for various functions

Common arguments and documentation for various functions

## Arguments

- region:

  sf simple features object. Shape file of the region of interest.

- update:

  Logical. Force update of the data?

- wells_sub:

  sf spatial data frame. Subset of wells data output by
  [`wells_subset()`](https://bcgov.github.io/bcaquiferdata/dev/reference/wells_subset.md)

- permission:

  Logical. Permission to create the cache folder. If `FALSE`, user is
  asked for permission, if `TRUE`, permission is implied.

## Details

Use `@inheritParams common_docs` to include the above in any function
documentation with a matching argument (will only include matching args)
