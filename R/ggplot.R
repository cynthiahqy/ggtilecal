#' Calculate variables for calendar layout
#'
#' Helper function for calculating calendar layout variables.
#'
#' @inheritParams lubridate::wday
#' @param .data tibble or data frame containing dates to be plotted in calendar layout
#' @param date_col column containing calendar unit dates
#'
#' @return tibble with additional calendar layout variables
#' @export
#'
#' @examples
#' make_empty_month_days(c("2024-01-01", "2024-02-01")) |> 
#'   calc_calendar_vars(unit_date)
#' @importFrom lubridate year month day wday mday floor_date
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

#' A modified version of `ggplot2::theme_bw()` for calendar layouts
#'
#' @inheritParams ggplot2::theme_bw
#'
#' @export
#'
#' @importFrom ggplot2 theme theme_bw %+replace%
#' element_blank element_rect element_text
#' unit margin
theme_bw_tilecal <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size/22, 
                             base_rect_size = base_size/22){
  theme_bw(
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size
  ) %+replace% 
    theme(
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

#' Make Monthly Calendar Facets
#'
#' Generates calendar with monthly facets by:
#' - Padding event list with any missing days via `fill_missing_units()`
#' - Calculating variables for calendar layout via `calc_calendar_vars()`
#' - Returning a ggplot object as per Details.
#' 
#' Returns a ggplot with the following fixed layers using calculated layout variables:
#' - `aes()` mapping:
#'    - `x` is day of week, `y` is week in month, `label` is day of month
#' - `facet_wrap()` by month
#' - `labs()` to remove axis labels for calculated layout variables
#' 
#' and default customisable layers:
#' - `geom_tile()`, `geom_text()` to label each day which inherit calculated variables
#' - `scale_y_reverse()` to order day in month correctly
#' - `scale_x_discrete()` to position weekday labels
#' - `coord_fixed()` to square each tile
#' - `theme_bw_tilecal()` to apply sensible theme defaults
#' 
#' To modify layers alter the `.geom` and `.scale_coord`,
#' which inherit the calculate layout mapping by default
#' (via the ggplot2 `inherit.aes` argument).
#' 
#' To add layers use the ggplot `+` function as normal,
#' or pass layers to the `.layers` argument.
#' This can be used to add interactive geoms (e.g. from `ggiraph`)
#' 
#' To modify the theme, use the ggplot `+` function as normal,
#' or add additional elements to the list in `.theme`.
#' 
#' To remove any of the optional layers, set the argument to any empty `list()`
#'
#' @inheritParams ggplot2::facet_wrap
#' @inheritParams fill_missing_units
#' @inheritParams calc_calendar_vars
#' @param .geom,.scale_coord,.theme,.layers
#'    Customisable lists of ggplot2 layers to add to the plot.
#'    An empty `list()` leaves the plot unmodified.
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
#' scale_y_reverse scale_x_discrete coord_fixed vars
gg_facet_wrap_months <- function(.events_long, date_col,
                                 locale = NULL, week_start = NULL,
                                 nrow = NULL, ncol = NULL,
                                 .geom = list(geom_tile(color = "grey70",
                                                        fill = "transparent"), 
                                               geom_text(nudge_y = 0.25)),
                                 .scale_coord = list(
                                   scale_y_reverse(),
                                   scale_x_discrete(position = "top"),
                                   coord_fixed(expand = TRUE)
                                 ),
                                 .theme = list(theme_bw_tilecal()),
                                 .layers = list()) {
  
  cal_data <- fill_missing_units(.events_long, {{ date_col }}) |>
    calc_calendar_vars({{date_col}})
  

  base_plot <- cal_data |>
    ggplot2::ggplot(mapping = aes(x = TC_wday_label,
                                  y = TC_month_week,
                                  label = TC_mday)) +
    facet_wrap(vars(TC_month_label), axes = "all_x", nrow = nrow, ncol = ncol) +
    labs(y = NULL, x = NULL) +
    .geom +
    .scale_coord +
    .theme
  
  base_plot
}
