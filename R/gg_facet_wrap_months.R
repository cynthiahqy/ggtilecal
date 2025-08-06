#' Make Monthly Calendar Facets
#'
#' Generates calendar with monthly facets by:
#' - Padding event list with any missing days via `fill_missing_units()`
#' - Calculating variables for calendar layout via `calc_calendar_vars()`
#' - Returning a ggplot object as per Details.
#' 
#' Returns a ggplot with the following fixed components
#' using calculated layout variables:
#' - `aes()` mapping:
#'    - `x` is day of week,
#'    - `y` is week in month,
#'    - `label` is day of month
#' - `facet_wrap()` by month
#' - `labs()` to remove axis labels for calculated layout variables
#' 
#' for the month facets: 
#'  - `nrow` the number of month facet rows 
#'  - or `ncol` the number of month facet columns
#'  - a labeller function for the monthly facet strip labels, see `label_yearmonth()`
#' 
#' and default customisable components:
#' - `geom_tile()`, `geom_text()` to label each day which inherit calculated variables
#' - `scale_y_reverse()` to order day in month correctly
#' - `scale_x_discrete()` to position weekday labels
#' - `coord_fixed()` to square each tile
#' - `theme_bw_tilecal()` to apply sensible theme defaults
#'
#' To modify components alter the `.geom` and `.scale_coord`,
#' which inherit the calculate layout mapping by default
#' (via the ggplot2 `inherit.aes` argument).
#'
#' To additional components use the ggplot `+` function as normal,
#' or pass components to the `.other` argument.
#' This can be used to add interactive geoms (e.g. from `ggiraph`)
#'
#' To modify the theme, use the ggplot `+` function as normal,
#' or add additional elements to the list in `.theme`.
#'
#' To remove any of the optional components, set the argument to any empty `list()`
#'
#' @inheritParams ggplot2::facet_wrap
#' @inheritParams fill_missing_units
#' @inheritParams calc_calendar_vars
#' @param .geom,.scale_coord,.theme,.other
#'    Customisable lists of ggplot2 components to add to the plot.
#'    An empty `list()` leaves the plot unmodified.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' demo_events_gpt |>
#'   reframe_events(startDate, endDate) |>
#'   group_by(unit_date) |>
#'   slice_min(order_by = duration) |>
#'   gg_facet_wrap_months(unit_date) +
#'   geom_text(aes(label = event_emoji), nudge_y = -0.25, na.rm = TRUE)
#' @importFrom ggplot2 aes_string geom_tile geom_text facet_wrap labs
#' scale_y_reverse scale_x_discrete coord_fixed vars
gg_facet_wrap_months <- function(.events_long, date_col,
                                 locale = NULL, week_start = NULL,
                                 nrow = NULL, ncol = NULL,
                                 labeller=label_yearmonth("%b"),
                                 .geom = list(
                                   geom_tile(
                                     color = "grey70",
                                     fill = "transparent"
                                   ),
                                   geom_text(nudge_y = 0.25)
                                 ),
                                 .scale_coord = list(
                                   scale_y_reverse(),
                                   scale_x_discrete(position = "top"),
                                   coord_fixed(expand = TRUE)
                                 ),
                                 .theme = list(theme_bw_tilecal()),
                                 .other = list()) {
  ## warn if duplicate date tiles --> overplotting

  cal_data <- .events_long |>
    fill_missing_units({{ date_col }}) |>
    calc_calendar_vars({{ date_col }})

  base_plot <- cal_data |>
    ggplot2::ggplot(mapping = aes_string(
      x = "TC_wday_label",
      y = "TC_month_week",
      label = "TC_mday"
    )) +
    facet_wrap(c("TC_month_label"), axes = "all_x", nrow = nrow, ncol = ncol, labeller=labeller) +
    labs(y = NULL, x = NULL) +
    .geom +
    .scale_coord +
    .theme +
    .other

  base_plot
}
