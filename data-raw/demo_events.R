library(yaml)
library(tibble)
library(dplyr)
library(emoji)
library(lorem)

# ----- demo_events_gpt -----

demo_events_gpt <- yaml::read_yaml("data-raw/demo_events_gpt.yml") |>
  lapply(as.data.frame) |>
  dplyr::bind_rows() |>
  tibble::as_tibble() |>
  dplyr::mutate(across(ends_with("Date"), ~ lubridate::ymd(.x))) |>
  dplyr::arrange(startDate) |>
  dplyr::mutate(duration = difftime(endDate, startDate, unit = "days") + 1)

usethis::use_data(demo_events_gpt, overwrite = TRUE)

# ----- random_demo_events() ------

#' Generate tibble of demo events
#'
#' `emoji` and `details` columns are only generated if the
#' `emoji` and `lorem` packages are installed
#'
#' @param n_events number of events to generate
#' @param date_range date range for events
#' @param max_duration maximum duration of an event
#'
#' @return tibble
#'
#' @examples
#' demo_events(30)
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
  event_duration <- sapply(event_max_durations + 1, sample.int, size = 1)
  
  events <- tibble::tibble(
    event_id = seq(n_events),
    title = paste("Event", seq(n_events)),
    start = event_start_dates,
    # max_duration = event_max_durations,
    # rand_duration = event_duration,
    end = event_start_dates + event_duration - 1,
    duration = event_duration
  )
  
  if (rlang::is_installed("emoji")) {
    events <- events |>
      dplyr::mutate(emoji = emoji::zoo(n_events, replace = TRUE))
  }
  if (rlang::is_installed("lorem")) {
    events <- events |>
      dplyr::rowwise() |>
      dplyr::mutate(details = unlist(lorem::ipsum(1, 1)))
  }
  
  events
}

set.seed(498)
demo_events_overlap <- demo_events(10, date_range=c("2024-01-01", "2024-03-31"))

usethis::use_data(demo_events_overlap, overwrite = TRUE)
