# Create Excel Drawdown template file

**This function is still in development. Use at your own risk!** Creates
an Excel file with formulas for calculating drawdown. File is pre-filled
with wells within 100km of `location` (well or coordinates).

## Usage

``` r
drawdown(
  location,
  rate = NA,
  duration = NA,
  overwrite = FALSE,
  file_name = NULL,
  update = FALSE
)
```

## Arguments

- location:

  Numeric. Either a vector with longitude/latitude, or a well tag
  number.

- rate:

  Numeric. The pumping rate in m3/day (defaults to `NA`, fillable in
  Excel file).

- duration:

  Numeric. The duration of pumping in days (defaults to `NA`, fillable
  in Excel file).

- overwrite:

  Logical. Overwrite existing file?

- file_name:

  Character. Optional file name for created excel file.

- update:

  Logical. Force update of the data?

## Value

Creates excel file

## Examples

``` r
if (FALSE) { # interactive()
drawdown(85199, rate = 343, duration = 180)
drawdown(85199, rate = 343, duration = 180, overwrite = TRUE)
drawdown(22966, rate = 343, duration = 180)
drawdown(22966)

drawdown(c(-123.5593, 48.647), rate = 3.97, duration = 180)
drawdown(c(-123.5593, 48.647))
}
```
