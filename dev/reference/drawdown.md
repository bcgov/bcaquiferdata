# Create Excel Drawdown template file

Creates an Excel file with formulas for calculating drawdown. File is
pre-filled with wells within 100km of `location` (well or coordinates).

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

- update:

  Logical. Force update of the data?

## Value

Creates excel file

## Examples

``` r
drawdown(85199, rate = 343, duration = 180)
#> bcaquiferdata would like to store data in: 
#> ~/.local/share/bcaquiferdata
#> Is that okay? (You can always use cache_clean() to remove it) (Yes/no/cancel) 
#> Creating cache directory: ~/.local/share/bcaquiferdata
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection
drawdown(85199, rate = 343, duration = 180, overwrite = TRUE)
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection
drawdown(22966, rate = 343, duration = 180)
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection
drawdown(22966)
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection

drawdown(c(-123.5593, 48.647), rate = 3.97, duration = 180)
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection
drawdown(c(-123.5593, 48.647))
#> Warning: cannot open file '/home/runner/.local/share/bcaquiferdata/wells_sf_nice.rds': No such file or directory
#> Error in readRDS(con, refhook = refhook): cannot open the connection
```
