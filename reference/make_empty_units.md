# Make empty sequence of calendar units between specified date range

Make empty sequence of calendar units between specified date range

## Usage

``` r
make_empty_units(cal_range, dates_to = "unit_date", cal_unit = "day")
```

## Arguments

- cal_range:

  `c(start,end)` range for which to create sequence of calendar units

- dates_to:

  string name for column to output calendar unit dates

- cal_unit:

  increment of calendar sequence passed to `by` argument in
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html)

## Value

tibble

## Examples

``` r
make_empty_units(c("2024-03-05", "2024-04-15"))
#> # A tibble: 42 × 1
#>    unit_date 
#>    <date>    
#>  1 2024-03-05
#>  2 2024-03-06
#>  3 2024-03-07
#>  4 2024-03-08
#>  5 2024-03-09
#>  6 2024-03-10
#>  7 2024-03-11
#>  8 2024-03-12
#>  9 2024-03-13
#> 10 2024-03-14
#> # ℹ 32 more rows
```
