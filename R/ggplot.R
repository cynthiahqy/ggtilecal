requireNamespace("ggplot2", quietly = TRUE)

add_styling_layers <- function(.ggplot) {
  list(
    ggplot2::scale_y_reverse(),
    ggplot2::coord_fixed(expand = TRUE),
  )
}

set_scale_coord <- function() {
  list(
    ggplot2::scale_y_reverse(),
    ggplot2::scale_x_discrete(position = "top"),
    ggplot2::coord_fixed(expand = TRUE)
  )
}

#' @importFrom ggplot2 element_blank element_rect element_text %+replace%
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

## base calendar
#' Title
#'
#' Generates base ggtilecal with monthly facets by:
#' - Padding event list with any missing days
#' - Calculating variables for calendar layout
#' Returns a ggplot with the following layers specified (in order):
#' - `aes()` mapping using calculated vars:
#'    - `x` is day of week, `y` is week in month
#' - `geom_tile()`, `geom_text()` to label each day
#' - `facet_wrap()` by month
#' - `labs()` to remove calculated vars axis labels
#' as well as (by default with `.set_scale_coord`):
#' - `scale_y_reverse()` to order day in month correctly
#' - `scale_x_discrete()` to position weekday labels
#' - `coord_fixed()` to square each tile
#'
#' To modify
#'
#' @param .events_long
#' @param date_col
#' @param cal_range
#' @param unit
#' @param locale
#' @param week_start
#' @param nrow
#' @param ncol
#' @param .set_scale_coord
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' demo_events(20) |>
#'   mutate(event_duration = difftime(end, start, unit = "days")) |>
#'   reframe_intervals(start, end) |>
#'   group_by(unit_date) |>
#'   arrange(unit_date) |>
#'   remove_overlaps(unit_date, event_duration, "first") |>
#'   gg_facet_wrap_months(unit_date)
#' @importFrom ggplot2 aes geom_tile geom_text facet_wrap labs
gg_facet_wrap_months <- function(.events_long, date_col, cal_range = NULL,
                                 unit = "day", locale = NULL, week_start = NULL,
                                 nrow = NULL, ncol = NULL, .set_scale_coord = TRUE) {
  # date_col_str <- rlang::englue("{{date_col}}")
  # if(anyDuplicated(.events_long[[date_col_str]])){
  #   rlang::warn("More than one event per {.var {unit}}")
  # }

  cal_data <- fill_cal_list(.events_long, {{ date_col }})

  # updates global defaults :(
  ## ggplot2::update_geom_defaults("tile", list(fill = "transparent", color = "grey70", alpha = 0.2))

  base_plot <- cal_data$.merged_units |>
    ggplot2::ggplot(mapping = aes(x = ct_wday_label, y = ct_month_week)) +
    geom_tile() +
    geom_text(aes(label = ct_mday), nudge_y = 0.25) +
    facet_wrap(vars(ct_month_label), axes = "all_x", nrow = nrow, ncol = ncol) +
    labs(y = NULL, x = NULL)

  if (.set_scale_coord) {
    base_plot +
      set_scale_coord()
  } else {
    base_plot
  }
}


if (FALSE) {
  full_cal_merged <- bind_rows(full_cal_list)

  g_base <- ggplot(
    data = full_cal_merged,
    mapping = aes(x = ct_wday_label, y = ct_month_week)
  ) +
    geom_tile(alpha = 0.2, color = "grey70") +
    facet_wrap(vars(ct_month_label), axes = "all_x") +
    geom_text(aes(label = ct_mday), nudge_y = 0.25) +
    scale_y_reverse() +
    scale_x_discrete(position = "top")

  g_base_themed <- g_base +
    layers_theme +
    theme(strip.placement = "outside") +
    coord_equal()

  g_base_themed +
    geom_text(
      data = full_cal_list$.event_units,
      mapping = aes(label = emoji)
    )

  # ggsave("static.png")
}
