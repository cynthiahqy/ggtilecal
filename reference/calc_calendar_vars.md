# Calculate variables for calendar layout

Helper function for calculating calendar layout variables.

## Usage

``` r
calc_calendar_vars(
  .data,
  date_col,
  locale = Sys.getlocale("LC_TIME"),
  week_start = 1
)
```

## Arguments

- .data:

  tibble or data frame containing dates to be plotted in calendar layout

- date_col:

  column containing calendar unit dates

- locale:

  locale to use for day names. Default to current locale.

- week_start:

  day on which week starts following ISO conventions: 1 means Monday and
  7 means Sunday (default). When `label = FALSE` and `week_start = 7`,
  the number returned for Sunday is 1, for Monday is 2, etc. When
  `label = TRUE`, the returned value is a factor with the first level
  being the week start (e.g. Sunday if `week_start = 7`). You can set
  `lubridate.week.start` option to control this parameter globally.

## Value

Tibble with additional calendar layout variables: `TC_year`,
`TC_month_label`, `TC_mday`, `TC_wday_label`, `TC_wday`,
`TC_month_week`, `TC_is_weekend`

## Examples

``` r
make_empty_month_days(c("2024-01-01", "2024-02-01")) |>
  calc_calendar_vars(unit_date)
#> # A tibble: 60 × 8
#>    unit_date  TC_year TC_month_label TC_mday TC_wday_label TC_wday TC_month_week
#>    <date>       <dbl>          <mth>   <int> <ord>           <dbl>         <dbl>
#>  1 2024-01-01    2024       2024 Jan       1 Mon                 1             1
#>  2 2024-01-02    2024       2024 Jan       2 Tue                 2             1
#>  3 2024-01-03    2024       2024 Jan       3 Wed                 3             1
#>  4 2024-01-04    2024       2024 Jan       4 Thu                 4             1
#>  5 2024-01-05    2024       2024 Jan       5 Fri                 5             1
#>  6 2024-01-06    2024       2024 Jan       6 Sat                 6             1
#>  7 2024-01-07    2024       2024 Jan       7 Sun                 7             1
#>  8 2024-01-08    2024       2024 Jan       8 Mon                 1             2
#>  9 2024-01-09    2024       2024 Jan       9 Tue                 2             2
#> 10 2024-01-10    2024       2024 Jan      10 Wed                 3             2
#> # ℹ 50 more rows
#> # ℹ 1 more variable: TC_is_weekend <lgl>
```
