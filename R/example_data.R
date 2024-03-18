# usethis::use_package("lubridate")
# usethis::use_package("charlatan", "Suggest")


event_fields <- c("startDate", "endDate", "details", "emoji", "link")

yaml_node_template <- function(fields, n, ...) {
    node <- vector("list", length(fields))
    # node <- as.list(rep(NA_character_, length(fields)))
    names(node) <- fields
    nodes <- rep(list(node), n)
    dots <- list(...)
    args <- c(list(x = nodes), dots)
    do.call(yaml::as.yaml, args)
}

parse_event_yaml <- function(string) {

}

#' Default value for `NULL`.
#'
#' @name null-default
#' @aliases %||%
#' @importFrom rlang %||%
NULL

demo_events <- function(
    n_events = 5,
    date_range = NULL,
    max_duration = 5,
    format = c("yaml", "df")) {
    requireNamespace("charlatan", quietly = TRUE)
    if (is.null(date_range)) {
        start_date <- lubridate::today()
        end_date <- lubridate::add_with_rollback(start_date, months(3))
    } else {
        start_date <- anytime::anydate(date_range[1])
        end_date <- anytime::anydate(date_range[2])
        stopifnot(end_date > start_date)
    }
    total_duration <- as.integer(difftime(end_date, start_date))
    event_day_no <- sort(sample.int(total_duration, n_events))
    event_start_dates <- start_date + as.integer(event_day_no)
    event_max_durations <- pmin(
        max_duration,
        as.integer(difftime(end_date, event_start_dates, units = "days"))
    )
    # 50% chance of one-day events!
    # event_duration <- sample(c(0, 1), n_events, replace = TRUE) * sapply(event_max_durations, sample.int, size = 1)
    event_duration <- sapply(event_max_durations + 1, sample.int, size = 1) - 1
    tibble::tibble(
        start = event_start_dates,
        # max_duration = event_max_durations,
        # duration = event_duration,
        end = event_start_dates + event_duration,
        details = paste("Event", seq(n_events)),
        emoji = emoji::zoo(n_events)
    )


    # dtp <- charlatan::DateTimeProvider$new()
    # as.Date(dtp$date_time_between(start_date, end_date))
}

fill_calendar <- function(event_days, padding_days) {
    padding_days |>
        anti_join(event_days, by = "date") |>
        bind_rows(event_days) |>
        mutate(
            Year = year(date),
            Month = month(date, label = TRUE),
            Day = wday(date, label = TRUE, week_start = 1),
            mday = mday(date),
            Month_week = (5 + day(date) +
                wday(floor_date(date, "month"), week_start = 1)) %/% 7,
            wday_no = wday(date)
        ) |>
        mutate(
            is_weekend = ifelse(wday_no %in% c(7, 1), TRUE, FALSE)
        ) |>
        arrange(date)
}

# This works!
# date_handler <- list(Date = function(x) as.character(x))
## yaml::as.yaml(list(date = as.Date("2012-10-10")), handlers = date_handler)
# So does this
## my_tbl <- tibble::tibble(xdate = as.Date("2012-10-10"))
## yaml::as.yaml(my_tbl, handlers = date_handler)
# But not this
## yaml::as.yaml(my_tbl, column.major = FALSE, handlers = date_handler)
## This also does the weird number conversion
# purrr::transpose(my_tbl)
## but not if you do as.list() first
# as.list(my_tbl) |> purrr::list_transpose(simplify = FALSE)
## what about logical handlers?
# my_tbl$bool <- c(TRUE, FALSE)
# verbatim_logical <- function(x) {
#   result <- tolower(as.logical(x))
#   class(result) <- "verbatim"
#   return(result)
# }
# yml_handlers <- c(date_handler, list(logical = verbatim_logical))
## BAD
# my_tbl |> yaml::as.yaml(handlers = yml_handlers, column.major = FALSE)
## Still
# my_tbl |> yaml::as.yaml(handlers = yml_handlers, column.major = TRUE)
