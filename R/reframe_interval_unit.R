# used

reframe_intervals <- function(.data, start, end, unit = "day") {
    if (dplyr::is_grouped_df(.data)) {
        x_grpd <- .data
    } else {
        interval_vars <- sapply(rlang::enquos(start, end), rlang::as_name)
        grouping_vars <- setdiff(names(.data), interval_vars)
        cli::cli_inform("Attempting row-wise grouping by: {.var {grouping_vars}}")
        x_grpd <- .data |>
            dplyr::group_by(dplyr::pick(tidyselect::all_of(grouping_vars)))
    }

    # check for dups (otherwise reframe will throw an error)
    dups <- x_grpd |> dplyr::filter(n() > 1)
    stopifnot(nrow(dups) == 0) # rowwise grouping didnt work

    x_grpd |>
        dplyr::reframe(
            unit_date = seq({{ start }}, {{ end }}, by = unit)
        )
}


# archive
pivot_longer_interval <- function(
    x, start_dt, end_dt,
    .by = NULL, unit = "day") {
    interval_vars <- c(rlang::englue("{{ start_dt }}"), rlang::englue("{{ end_dt }}"))
    print()
    group_vars <- `.by` %||%
        cli::cli_inform("Grouping by: {.var {group_vars}}")
    x |>
        dplyr::group_by(dplyr::pick(tidyselect::all_of(group_vars))) |>
        dplyr::reframe(
            unit_dt = seq({{ start_dt }}, {{ end_dt }}, by = unit)
        )
}

reframe_interval_units <- function(
    .data, start, end, unit = "day", .by = NULL) {
    grouping_vars <- dplyr::group_vars(.data)
    if (!is_empty(grouping_vars)) {
        x_grpd <- .data
        print(x_grpd)
    } else if (is.null(`.by`) && is_empty(grouping_vars)) {
        interval_vars <- sapply(rlang::enquos(start_dt, end_dt), rlang::as_name)
        grouping_vars <- setdiff(names(.data), interval_vars)
        x_grpd <- .data |>
            dplyr::group_by(grouping_vars)
    } else {
        by <- compute_by({{ .by }}, .data, by_arg = ".by", data_arg = ".data")
        print(by)
    }

    # check for dups (otherwise reframe will throw an error)
    dups <- x_grpd |> dplyr::filter(n() > 1)
    stopifnot(nrow(dups) == 0)

    x_grpd |>
        dplyr::reframe(
            unit_dt = seq({{ start_dt }}, {{ end_dt }}, by = unit)
        )
}
