#' Calculate variables for calendar layout
#'
#' Helper function for calculating calendar layout variables.
#'
#' @inheritParams lubridate::wday
#' @param .data tibble or data frame
#' containing dates to be plotted in calendar layout
#' @param date_col column containing calendar unit dates
#'
#' @return Tibble with additional calendar layout variables:
#' `TC_year`, `TC_month_label`, `TC_mday`,
#' `TC_wday_label`, `TC_wday`, `TC_month_week`,
#' `TC_is_weekend`
#' @export
#'
#' @examples
#' make_empty_month_days(c("2024-01-01", "2024-02-01")) |>
#'   calc_calendar_vars(unit_date)
#' @importFrom lubridate year month day wday mday floor_date
#' @importFrom tsibble yearmonth
calc_calendar_vars <- function(.data, date_col,
                               locale = Sys.getlocale("LC_TIME"),
                               week_start = 1) {
  .data |> dplyr::mutate(
    TC_year = year({{ date_col }}),
    TC_month_label = yearmonth({{ date_col }}),
    TC_mday = mday({{ date_col }}),
    TC_wday_label = wday({{ date_col }},
      label = TRUE,
      locale = locale,
      week_start = week_start
    ),
    TC_wday = wday({{ date_col }}, week_start = week_start),
    TC_month_week = (5 + day({{ date_col }}) +
      wday(floor_date({{ date_col }}, "month"), week_start = week_start)) %/% 7,
    TC_is_weekend =
      ifelse(wday({{ date_col }}, week_start = 1) %in% c(6, 7), TRUE, FALSE)
  )
}
