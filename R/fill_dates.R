# w/o events

#' Make empty sequence of calendar units between specified date range
#'
#' @param cal_range `c(start,end)` range for which to create sequence of calendar units
#' @param dates_to string name for column to output calendar unit dates
#' @param cal_unit increment of calendar sequence passed to `by` argument in \code{\link[base]{seq.Date}}
#'
#' @return tibble
#' @export
#'
#' @examples make_empty_units(c("2024-03-05", "2024-04-15"))
make_empty_units <- function(cal_range, dates_to = "unit_date", cal_unit = "day") {
  cal_range <- anytime::anydate(cal_range)
  start_dt <- min(cal_range)
  end_dt <- max(cal_range)
  
  x <- tibble::tibble(
        start = start_dt,
        end = end_dt
    )
    x |>
        dplyr::reframe("{ dates_to }" := seq(start, end, by = cal_unit))
}


#' Make padding day unit entries for specified months
#'
#' Creates empty day units using `make_empty_units()` that span the months
#' encompassing `cal_range` with the option to adjust the number of months
#' either side of `cal_range`
#' 
#' @inheritParams make_empty_units
#' @param adjust_months vector specifying how many months to add
#'
#' @return
#' @export
#'
#' @examples make_months(c("2024-03-05", "2024-04-15"))
make_empty_month_days <- function(
    month_range,
    dates_to = "unit_date",
    adjust_months = c(-0, 0)) {
  
    stopifnot(adjust_months[[1]] <= 0)
    stopifnot(adjust_months[[2]] >= 0)
  
    month_range <- anytime::anydate(month_range)
    start_dt <- min(month_range)
    end_dt <- max(month_range)

    start_month <- lubridate::add_with_rollback(
        e1 = lubridate::floor_date(start_dt, unit = "month"),
        e2 = months(adjust_months[[1]])
    )
    end_month <- lubridate::add_with_rollback(
        e1 = lubridate::rollforward(end_dt),
        e2 = months(adjust_months[[2]])
    )

    make_empty_units(c(start_month, end_month),
        dates_to = dates_to, cal_unit = "day"
    )
}

#' Fill out event calendar with missing days
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
#'
#' @param .data
#' @param adjust_months 
#' @param cal_unit 
#' @param date_col
#'
#' @return tibble
#' @export
#'
#' @examples 
fill_missing_units <- function(.data, date_col, adjust_months = NULL,
                               cal_unit = "day") {
  filled <- fill_missing_units_list(.data, {{date_col}}, adjust_months, cal_unit)
  filled[['.merged_units']]
}
