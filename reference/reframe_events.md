# Reframe event intervals to calendar units

**\[experimental\]**

## Usage

``` r
reframe_events(
  .data,
  event_start,
  event_end,
  id_cols = NULL,
  dates_to = "unit_date",
  cal_unit = "day"
)
```

## Arguments

- .data:

  A data frame or tibble containing event details

- event_start:

  column containing event start date

- event_end:

  column containing event end date

- id_cols:

  (optional) set of columns that uniquely identify each observation.
  Ignored if `.data` is grouped (i.e. using
  [group_by](https://dplyr.tidyverse.org/reference/group_by.html)). If
  `.data` is not grouped and `id_cols` is `NULL`, all columns except for
  `event_start` and `event_end` are used.

- dates_to:

  string name for column to output calendar unit dates

- cal_unit:

  increment of calendar sequence passed to `by` argument in
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html)

## Value

See return value of
[reframe](https://dplyr.tidyverse.org/reference/reframe.html)

## Details

Convert rows of events to sequence of calendar units for plotting.
Duplicates all columns except for `event_start` and `event_date`.
Requires a unique id key for each event.

This is a thin wrapper around
[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
that expands each uniquely identified event in `.data` to a sequence of
datetimes defined by the `start`, `end` and the interval `cal_unit`.

## Examples

``` r
event <- data.frame(
  id = 1,
  start = as.Date("2024-01-05"),
  end = as.Date("2024-01-10")
)
reframe_events(event, start, end)
#> # A tibble: 6 × 2
#>      id unit_date 
#>   <dbl> <date>    
#> 1     1 2024-01-05
#> 2     1 2024-01-06
#> 3     1 2024-01-07
#> 4     1 2024-01-08
#> 5     1 2024-01-09
#> 6     1 2024-01-10
demo_events_gpt |>
  reframe_events(startDate, endDate)
#> # A tibble: 15 × 7
#>    event_id event_title   event_descr event_emoji event_link duration unit_date 
#>    <chr>    <chr>         <chr>       <chr>       <chr>      <drtn>   <date>    
#>  1 EVT001   TechFest 2024 TechFest 2… 🤖          https://t… 2 days   2024-05-15
#>  2 EVT001   TechFest 2024 TechFest 2… 🤖          https://t… 2 days   2024-05-16
#>  3 EVT002   Global Healt… The Global… 🌍          https://g… 3 days   2024-05-25
#>  4 EVT002   Global Healt… The Global… 🌍          https://g… 3 days   2024-05-26
#>  5 EVT002   Global Healt… The Global… 🌍          https://g… 3 days   2024-05-27
#>  6 EVT003   Sustainable … The Sustai… 🌱          https://s… 3 days   2024-06-10
#>  7 EVT003   Sustainable … The Sustai… 🌱          https://s… 3 days   2024-06-11
#>  8 EVT003   Sustainable … The Sustai… 🌱          https://s… 3 days   2024-06-12
#>  9 EVT004   Future of Wo… The Future… 💼          https://f… 2 days   2024-06-20
#> 10 EVT004   Future of Wo… The Future… 💼          https://f… 2 days   2024-06-21
#> 11 EVT005   Space Explor… The Space … 🚀          https://s… 5 days   2024-07-05
#> 12 EVT005   Space Explor… The Space … 🚀          https://s… 5 days   2024-07-06
#> 13 EVT005   Space Explor… The Space … 🚀          https://s… 5 days   2024-07-07
#> 14 EVT005   Space Explor… The Space … 🚀          https://s… 5 days   2024-07-08
#> 15 EVT005   Space Explor… The Space … 🚀          https://s… 5 days   2024-07-09
```
