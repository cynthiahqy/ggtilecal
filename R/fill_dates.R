# w/o events

make_calendar_units <- function(start_dt, end_dt, dates_to = "unit_date", unit = "day") {
    x <- tibble::tibble(
        start = anytime::anytime(start_dt),
        end = anytime::anytime(end_dt)
    )
    x |>
        dplyr::reframe("{ dates_to }" := seq(start, end, by = unit))
}


#' Make padding months/day unit entries
#'
#' @param start_dt 
#' @param end_dt 
#' @param dates_to 
#' @param expand 
#'
#' @return
#' @export
#'
#' @examples make_months("2024-03-05", "2024-04-15")
make_months <- function(
    start_dt, end_dt,
    dates_to = "unit_date",
    expand = c(-0, 0)) {
  
    stopifnot(expand[[1]] <= 0)
    stopifnot(expand[[2]] >= 0)
  
    start_dt <- anytime::anydate(start_dt)
    end_dt <- anytime::anydate(end_dt)

    start_month <- lubridate::add_with_rollback(
        e1 = lubridate::floor_date(start_dt, unit = "month"),
        e2 = months(expand[[1]])
    )
    end_month <- lubridate::add_with_rollback(
        e1 = lubridate::rollforward(end_dt),
        e2 = months(expand[[2]])
    )

    make_calendar_units(start_month, end_month,
        dates_to = dates_to, unit = "day"
    )
}

# test output has expected min/max dates

# demo_events(100) |>
#         mutate(event_duration = difftime(end, start, unit = "days")) |>
#         reframe_intervals(start, end) |>
#         group_by(unit_date) |>
#         arrange(unit_date) |>
#         remove_overlaps(unit_date, event_duration, "first") |>
#         fill_calendar(unit_date) |>
#         filter(is.na(event_id))

#' Remove "overlaps" i.e. select one event per day
#'
#' @param .data 
#' @param date_col 
#' @param sort_col 
#' @param method 
#'
#' @return
#'
#' @examples
slice_one_per_date <- function(
    .data, date_col, arrange_by,
    method = c("head", "tail", "sample")) {
    x <- .data |>
        dplyr::group_by({{ date_col }}) |>
        dplyr::arrange({{ arrange_by }})

    switch(method,
        head = {
            dplyr::slice_head(x, n = 1)
        },
        tail = {
            dplyr::slice_tail(x, n = 1)
        },
        sample = {
            dplyr::slice_sample(x, n = 1)
        }
    ) |> dplyr::ungroup()
}

calc_calendar_vars <- function(.data, date_col, locale = Sys.getlocale("LC_TIME"),  week_start = 1){
  .data |> dplyr::mutate(
    TC_year = year({{ date_col }}),
    TC_month_label = month({{ date_col }},
                           label = TRUE,
                           locale = locale
    ),
    TC_wday_label = wday({{ date_col }},
                         label = TRUE,
                         local = locale,
                         week_start = week_start
    ),
    TC_mday = mday({{ date_col }}),
    TC_wday = wday({{ date_col }}, week_start = week_start),
    TC_month_week = (5 + day({{ date_col }}) +
                       wday(floor_date({{ date_col }}, "month"), week_start = week_start)) %/% 7,
    TC_is_weekend = ifelse(wday({{ date_col }}, week_start = 1) %in% c(6, 7), TRUE, FALSE)
  )
}

if (FALSE){
  demo_events(20) |>
    reframe_intervals(start, end) |>
    slice_one_per_date(unit_date, duration, "head") |>
    arrange(unit_date)
}

# demo_events(20) |>
#     mutate(event_duration = difftime(end, start, unit = "days")) |>
#     reframe_intervals(start, end) |>
#     group_by(unit_date) |>
#     arrange(unit_date) |>
#     remove_overlaps(unit_date, event_duration, "first") |>
#     fill_cal_list(unit_date)
fill_cal_list <- function(.data, date_col, cal_range = NULL,
                          unit = "day", locale = Sys.getlocale("LC_TIME"), week_start = 1) {
  out <- list()
  date_col_str <- rlang::englue("{{date_col}}")
  x_dates <- .data[[rlang::englue("{{date_col}}")]]

  out$.padding_units <- make_months(
    min(x_dates), max(x_dates),
    dates_to = date_col_str,
    expand = c(0, 0)) |>
    dplyr::anti_join(.data, by = c(date_col_str)) |>
    calc_calendar_vars({{ date_col }})
  
  out$.event_units <- .data |>
    calc_calendar_vars({{ date_col }})
  
  out$.merged_units <- dplyr::bind_rows(out$.padding_units, out$.event_units)
  
  return(out)
}

#' Fill out event calendar with missing days
#'
#' @param .data
#' @param date_col
#' @param cal_range
#' @param unit
#'
#' @return
#' @export
#'
#' @examples 
#' library(dplyr)
#' demo_events(20) |>
#'   reframe_intervals(start, end) |>
#'   group_by(unit_date) |>
#'   slice_min(order_by = duration) |>
#'   fill_calendar(unit_date)
#'
#' @importFrom lubridate year month day wday mday floor_date
fill_calendar <- function(.data, date_col, cal_range = NULL,
                          unit = "day", locale = Sys.getlocale("LC_TIME"), week_start = 1) {
  ## TODO: overlaps check
  cal_list <- fill_cal_list(.data, {{date_col}}, cal_range,
                            unit = "day", locale = Sys.getlocale("LC_TIME"), week_start = 1)
  padding_units <- cal_list[[1]]
  padding_units |>
    dplyr::bind_rows(.data) |>
    dplyr::arrange({{ date_col }})
}
