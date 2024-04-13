#' Reframe event intervals to calendar units
#'
#' `r lifecycle::badge("experimental")`
#'
#' Convert rows of events to sequence of calendar units for plotting.
#' Duplicates all columns except for `event_start` and `event_date`.
#' Requires a unique id key for each event.
#'
#' This is a thin wrapper around `dplyr::reframe()` that expands 
#' each uniquely identified event in `.data` to a sequence of datetimes defined
#' by the `start`, `end` and the interval `cal_unit`. 
#'
#' @param .data A data frame or tibble containing event details
#' @param event_start column containing event start date
#' @param event_end column containing event end date
#' @param id_cols (optional) set of columns that uniquely identify each observation.
#' Ignored if `.data` is grouped (i.e. using \link[dplyr]{group_by}).
#' If `.data` is not grouped and `id_cols` is `NULL`, all columns except for
#' `event_start` and `event_end` are used.
#' @inheritParams make_empty_units
#'
#' @return See return value of \link[dplyr]{reframe}
#' @export
#' @importFrom dplyr group_by pick filter n group_vars
#'
#' @examples
#' event <- data.frame(
#'   id = 1,
#'   start = as.Date("2024-01-05"),
#'   end = as.Date("2024-01-10"))
#' reframe_events(event, start, end)
#' demo_events(5) |>
#'     reframe_events(start, end)
reframe_events <- function(
    .data, event_start, event_end, id_cols = NULL,
    dates_to = "unit_date", cal_unit = "day") {
      
    if (dplyr::is_grouped_df(.data)) {
        x_grpd <- .data
    } else if (!is.null(id_cols)) {
      x_grpd <- .data |>
        dplyr::group_by(pick({{id_cols}}))
    } else {
        interval_vars <- sapply(enquos(event_start, event_end), as_name)
        grouping_vars <- setdiff(names(.data), interval_vars)
        if (length(grouping_vars) == 0){
          cli::cli_abort(c(
            "No columns available for `id_cols`",
            "x" = "`.data` should have at least one extra column
            other than `start` and `end`"))
          }
        x_grpd <- .data |>
          dplyr::group_by(pick(tidyselect::all_of(grouping_vars)))
    }

    # check for dups (otherwise reframe will throw an error)
    dups <- x_grpd |> dplyr::filter(n() > 1)
    if (nrow(dups) != 0){
      grouping_vars <- dplyr::group_vars(x_grpd)
      cli::cli_abort(c("Rows must be uniquely identified.",
                       "i" = "Column{?s} {.var {grouping_vars}} do{?es/} not uniquely identify rows.",
                       "i" = "Modify `id_cols`."))
    }
    
    x_grpd |>
        dplyr::reframe(
            unit_date = seq({{ event_start }}, {{ event_end }}, by = cal_unit)
        )
}

if( FALSE){
  # missing id columns
  library(dplyr)
  demo_events(30) |>
    select(start,end) |>
    rowwise() |>
    reframe_events(start, end)
  
  demo_events(30) |>
    select(start, end) |>
    mutate(id = "same") |>
    reframe_events(start, end)
  
  demo_events(30) |>
    select(start, end) |>
    mutate(id = "same", id2 = "same") |>
    reframe_events(start, end)
}

