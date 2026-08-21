# Fill out long format event table with missing dates

Helper function for filling out event table with any missing calendar
units.

## Usage

``` r
fill_missing_units(
  .events_long,
  date_col,
  adjust_months = NULL,
  cal_unit = "day"
)
```

## Arguments

- .events_long:

  long format calendar event data

- date_col:

  column containing calendar unit dates

- adjust_months:

  how many months to add before and after

- cal_unit:

  increment of calendar sequence passed to `by` argument in
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html)

## Value

tibble

## Examples

``` r
demo_events_gpt |>
  reframe_events(startDate, endDate) |>
  fill_missing_units(unit_date)
#> # A tibble: 92 × 7
#>    unit_date  event_id event_title event_descr event_emoji event_link duration
#>    <date>     <chr>    <chr>       <chr>       <chr>       <chr>      <drtn>  
#>  1 2024-05-01 NA       NA          NA          NA          NA         NA days 
#>  2 2024-05-02 NA       NA          NA          NA          NA         NA days 
#>  3 2024-05-03 NA       NA          NA          NA          NA         NA days 
#>  4 2024-05-04 NA       NA          NA          NA          NA         NA days 
#>  5 2024-05-05 NA       NA          NA          NA          NA         NA days 
#>  6 2024-05-06 NA       NA          NA          NA          NA         NA days 
#>  7 2024-05-07 NA       NA          NA          NA          NA         NA days 
#>  8 2024-05-08 NA       NA          NA          NA          NA         NA days 
#>  9 2024-05-09 NA       NA          NA          NA          NA         NA days 
#> 10 2024-05-10 NA       NA          NA          NA          NA         NA days 
#> # ℹ 82 more rows
```
