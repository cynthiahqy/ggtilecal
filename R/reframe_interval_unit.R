# .by argument & expand_to
pivot_longer_interval <- function(
    x, start_dt, end_dt,
    .by = NULL, unit = "day") {
    interval_vars <- c(rlang::englue("{{ start_dt }}"), rlang::englue("{{ end_dt }}"))
    print(sapply(rlang::enquos(start_dt, end_dt), rlang::as_name))
    group_vars <- `.by` %||% setdiff(names(x), interval_vars)
    cli::cli_inform("Grouping by: {.var {group_vars}}")
    x |>
        dplyr::group_by(dplyr::pick(tidyselect::all_of(group_vars))) |>
        dplyr::reframe(
            unit_dt = seq({{ start_dt }}, {{ end_dt }}, by = unit)
        )
}
