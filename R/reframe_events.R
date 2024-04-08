#' Reframe event intervals to calendar units
#'
#' `r lifecycle::badge("experimental")`
#'
#' Convert rows of events to sequence of calendar units for plotting.
#' Duplicates all columns except for `event_start` and `event_date`.
#'
#' This is a thin wrapper around `dplyr::reframe()`.
#'
#' @param .data A data frame or tibble containing event details
#' @param event_start column containing event start date
#' @param event_end column containing event end date
#' @inheritParams make_empty_units
#'
#' @return See return value of \link[dplyr]{reframe}
#' @export
#' @importFrom dplyr group_by pick filter n
#'
#' @examples
#' demo_events(5) |>
#'     reframe_events(start, end)
reframe_events <- function(
    .data, event_start, event_end,
    dates_to = "unit_date", cal_unit = "day") {
    if (dplyr::is_grouped_df(.data)) {
        x_grpd <- .data
    } else {
        interval_vars <- sapply(enquos(event_start, event_end), as_name)
        grouping_vars <- setdiff(names(.data), interval_vars)
        cli::cli_inform("Reframing using grouping by: {.var {grouping_vars}}")
        x_grpd <- .data |>
            dplyr::group_by(pick(tidyselect::all_of(grouping_vars)))
    }

    # check for dups (otherwise reframe will throw an error)
    dups <- x_grpd |> dplyr::filter(n() > 1)
    stopifnot(nrow(dups) == 0) # rowwise grouping didnt work

    x_grpd |>
        dplyr::reframe(
            unit_date = seq({{ event_start }}, {{ event_end }}, by = cal_unit)
        )
}
