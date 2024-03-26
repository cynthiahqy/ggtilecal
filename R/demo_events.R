#' Generate tibble of demo events
#'
#' `emoji` and `details` columns are only generated if the 
#' `emoji` and `lorem` packages are installed
#'
#' @param n_events 
#' @param date_range 
#' @param max_duration 
#' @param format 
#'
#' @return tibble
#' @export
#'
#' @examples
#' demo_events(30) |>
#'   dplyr::arrange(desc(duration))
demo_events <- function(
    n_events = 5,
    date_range = NULL,
    max_duration = 5) {
    if (is.null(date_range)) {
        start_date <- lubridate::today()
        end_date <- lubridate::add_with_rollback(start_date, months(3))
    } else {
        start_date <- anytime::anydate(date_range[1])
        end_date <- anytime::anydate(date_range[2])
    }
    stopifnot(end_date > start_date)

    total_duration <- as.integer(difftime(end_date, start_date))
    event_day_no <- sort(sample.int(total_duration, n_events, replace = TRUE))
    event_start_dates <- start_date + as.integer(event_day_no)
    event_max_durations <- pmin(
        max_duration,
        as.integer(difftime(end_date, event_start_dates, units = "days"))
    )
    event_duration <- sapply(event_max_durations + 1, sample.int, size = 1) - 1
    
    events <- tibble::tibble(
        event_id = seq(n_events),
        title = paste("Event", seq(n_events)),
        start = event_start_dates,
        # max_duration = event_max_durations,
        # rand_duration = event_duration,
        end = event_start_dates + event_duration,
        duration = difftime(end, start, unit = "days") + 1
    )
    
    if (rlang::is_installed("emoji")){
      events <- events |>
        dplyr::mutate(emoji = emoji::zoo(n_events, replace = TRUE))
    }
    if (rlang::is_installed("lorem")){
      events <- events |>
        dplyr::rowwise() |>
        dplyr::mutate(details = unlist(lorem::ipsum(1,1)))
    }
    
    events
}
