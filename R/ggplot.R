#' Calculate variables for calendar layout
#'
#' @param .data 
#' @param date_col 
#' @param locale 
#' @param week_start 
#'
#' @return tibble with additional calendar layout variables
#' @export
#'
#' @examples 
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

#' @importFrom ggplot2 element_blank element_rect element_text
#' unit margin %+replace%
theme_ggtilecal <- function(...) {
  # starts with theme_bw then modify
  ggplot2::theme_bw(...) %+replace%
    ggplot2::theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.y = unit(0, "lines"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_rect(fill = "grey95"),
      # strip.text = element_text(hjust = 0),
      strip.placement = "outside",
      complete = TRUE
    )
}

if (FALSE) {
  layers_theme <- list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.spacing = unit(0, "lines"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(hjust = 0),
      strip.placement = "outside"
    )
  )

  events_long <-
    demo_events(100) |>
    mutate(event_duration = difftime(end, start, unit = "days")) |>
    reframe_intervals(start, end) |>
    group_by(unit_date) |>
    arrange(unit_date) |>
    remove_overlaps(unit_date, event_duration, "first")

  base_plot <-
    gg_facet_wrap_months(events_long, unit_date)

  ## Why do these two things not return the same thing???
  base_plot + theme_ggtilecal()
  base_plot + layers_theme
}

#' Make Monthly Calendar Facets
#'
#' Generates calendar with monthly facets by:
#' - Padding event list with any missing days
#' - Calculating variables for calendar layout
#' - Returning a ggplot object as per Details.
#' 
#' Returns a ggplot with the following fixed layers specified (in order):
#' - `aes()` mapping using calculated vars:
#'    - `x` is day of week, `y` is week in month, `label` is day of month
#' - `facet_wrap()` by month
#' - `labs()` to remove calculated vars axis labels
#' 
#' and default customisable layers:
#' - `geom_tile()`, `geom_text()` to label each day
#' - `scale_y_reverse()` to order day in month correctly
#' - `scale_x_discrete()` to position weekday labels
#' - `coord_fixed()` to square each tile
#' - `theme_ggtilecal()` to apply sensible theme defaults
#' 
#' To modify layers use `.geom`, `.scale_coord`, `.theme` or `.layers`.
#' This can be used to add interactive geoms (e.g. from `ggiraph`)
#'
#' @inheritParams lubridate::wday
#' @inheritParams ggplot2::facet_wrap
#' @param .events_long
#' @param date_col
#' @param cal_range WIP/TODO
#' @param unit TODO rename to `tile_unit`
#' @param .geom,.scale_coord,.theme,.layers
#'    Customisable lists of ggplot2 layers to add to the plot.
#'    An empty list() leaves the plot unmodified.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' demo_events(20) |>
#'   reframe_intervals(start, end) |>
#'   group_by(unit_date) |>
#'   slice_min(order_by = duration) |>
#'   gg_facet_wrap_months(unit_date)
#' @importFrom ggplot2 aes geom_tile geom_text facet_wrap labs
#' scale_y_reverse scale_x_discrete coord_fixed
gg_facet_wrap_months <- function(.events_long, date_col, cal_range = NULL,
                                 unit = "day", locale = NULL, week_start = NULL,
                                 nrow = NULL, ncol = NULL,
                                 .geom = list(geom_tile(), 
                                               geom_text(nudge_y = 0.25)),
                                 .scale_coord = list(
                                   scale_y_reverse(),
                                   scale_x_discrete(position = "top"),
                                   coord_fixed(expand = TRUE)
                                 ),
                                 .theme = list(theme_ggtilecal()),
                                 .layers = list()) {
  # date_col_str <- rlang::englue("{{date_col}}")
  # if(anyDuplicated(.events_long[[date_col_str]])){
  #   rlang::warn("More than one event per {.var {unit}}")
  # }

  cal_data <- fill_cal_list(.events_long, {{ date_col }})

  base_plot <- cal_data$.merged_units |>
    ggplot2::ggplot(mapping = aes(x = TC_wday_label,
                                  y = TC_month_week,
                                  label = TC_mday)) +
    facet_wrap(vars(TC_month_label), axes = "all_x", nrow = nrow, ncol = ncol) +
    labs(y = NULL, x = NULL) +
    .geom +
    .scale_coord +
    .theme
}
