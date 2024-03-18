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
#' @export
#'
#' @examples
remove_overlaps <- function(
    .data, date_col, sort_col,
    method = c("first", "last", "random")) {
    x <- .data |>
        dplyr::group_by({{ date_col }}) |>
        dplyr::arrange({{ sort_col }})

    switch(method,
        first = {
            dplyr::slice_head(x, n = 1)
        },
        last = {
            dplyr::slice_tail(x, n = 1)
        },
        random = {
            dplyr::slice_sample(x, n = 1)
        }
    )
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
#' @examples demo_events(20) |>
#' mutate(event_duration = difftime(end, start, unit = "days")) |>
#' reframe_intervals(start, end) |>
#' group_by(unit_date) |>
#' arrange(unit_date) |>
#' remove_overlaps(unit_date, event_duration, "first") |>
#' fill_calendar(unit_date)
fill_calendar <- function(.data, date_col, cal_range = NULL, unit = "day") {
    date_col_str <- rlang::englue("{{date_col}}")

    if (dplyr::is_grouped_df(.data)) {
        grouping_vars <- dplyr::group_vars(.data)
    }
    other_cols <- setdiff(names(.data), date_col_str)

    dates <- .data[[rlang::englue("{{date_col}}")]]
    padding_units <- make_months(
        min(dates), max(dates),
        dates_to = date_col_str,
        expand = c(0, 0)
    )

    ## TODO: overlaps check

    padding_units |>
        dplyr::anti_join(.data, by = c(date_col_str)) |>
        dplyr::bind_rows(.data) |>
        dplyr::mutate(
            Year = lubridate::year({{ date_col }}),
            Month = lubridate::month({{ date_col }}, label = TRUE),
            Day = lubridate::wday({{ date_col }}, label = TRUE, week_start = 1),
            mday = lubridate::mday({{ date_col }}),
            Month_week = (5 + lubridate::day({{ date_col }}) +
                lubridate::wday(lubridate::floor_date({{ date_col }}, "month"), week_start = 1)) %/% 7,
            wday_no = lubridate::wday({{ date_col }})
        ) |>
        dplyr::mutate(
            is_weekend = ifelse(wday_no %in% c(7, 1), TRUE, FALSE)
        ) |>
        dplyr::arrange({{ date_col }})
}
