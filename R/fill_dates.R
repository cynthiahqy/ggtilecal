#' Fill out event calendar with missing days
#' @keywords internal
fill_missing_units_list <- function(.data, date_col, adjust_months = NULL,
                          cal_unit = "day") {
  out <- list()
  date_col_str <- rlang::englue("{{date_col}}")
  x_dates <- .data[[rlang::englue("{{date_col}}")]]
  
  out$.padding_units <- make_empty_month_days(
    month_range = c(min(x_dates), max(x_dates)),
    dates_to = date_col_str,
    adjust_months = c(0, 0)) |>
    dplyr::anti_join(.data, by = c(date_col_str))
  
  out$.event_units <- .data
  
  out$.merged_units <- dplyr::bind_rows(out$.padding_units, out$.event_units)
  
  return(out)
}

#' Fill out long format event table with missing dates
#'
#' Helper function for filling out event table with any missing calendar units.

#' @inheritParams make_empty_units
#' @inheritParams make_empty_month_days
#' @param .events_long long format calendar event data
#'
#' @return tibble
#' @export
#'
#' @examples
#' demo_events(1) |>
#'   reframe_events(start, end) |>
#'   fill_missing_units(unit_date)
fill_missing_units <- function(.data, date_col, adjust_months = NULL,
                               cal_unit = "day") {
  filled <- fill_missing_units_list(.data, {{date_col}}, adjust_months, cal_unit)
  filled[['.merged_units']]
}
