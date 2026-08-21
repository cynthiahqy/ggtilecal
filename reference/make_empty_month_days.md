# Make padding day unit entries for specified months

Creates empty day units using
[`make_empty_units()`](https://cynthiahqy.github.io/ggtilecal/reference/make_empty_units.md)
that span the months encompassing `cal_range` with the option to adjust
the number of months either side of `cal_range`

## Usage

``` r
make_empty_month_days(
  month_range,
  dates_to = "unit_date",
  adjust_months = c(-0, 0)
)
```

## Arguments

- month_range:

  vector of dates.

- dates_to:

  string name for column to output calendar unit dates

- adjust_months:

  how many months to add before and after

## Value

tibble with one row per day in specified months

## Examples

``` r
make_empty_month_days(c("2024-03-05", "2024-04-15"))
#> # A tibble: 61 × 1
#>    unit_date 
#>    <date>    
#>  1 2024-03-01
#>  2 2024-03-02
#>  3 2024-03-03
#>  4 2024-03-04
#>  5 2024-03-05
#>  6 2024-03-06
#>  7 2024-03-07
#>  8 2024-03-08
#>  9 2024-03-09
#> 10 2024-03-10
#> # ℹ 51 more rows
```
