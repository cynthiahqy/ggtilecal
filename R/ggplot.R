requireNamespace("ggplot2", quietly = TRUE)

add_styling_layers <- function(){
  list(
    ggplot2::scale_y_reverse(),
    ggplot2::coord_fixed(expand = TRUE),
    ggplot2::labs(y = NULL, x = NULL)
  )
}

layers_scale_coord <- function(){
  list(
  ggplot2::scale_y_reverse(),
  ggplot2::coord_fixed(expand = TRUE)
)
}

#' @importFrom ggplot2 element_blank element_rect element_text
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
    strip.text = element_text(hjust = 0)
  )
)

# full_calendar <-
#   demo_events(100) |>
#   mutate(event_duration = difftime(end, start, unit = "days")) |>
#   reframe_intervals(start, end) |>
#   group_by(unit_date) |>
#   arrange(unit_date) |>
#   remove_overlaps(unit_date, event_duration, "first") |>
#   fill_calendar(unit_date)

full_calendar |>
  ggplot(aes(x = ct_wday_label, y = ct_month_week)) +
  geom_tile(alpha = 0.2, color = "grey70") +
  geom_text(aes(label = ct_mday), nudge_y = 0.25) +
  geom_text(aes(label = emoji), nudge_y = -0.25) +
  scale_x_discrete(position = "top") +
  facet_wrap(vars(ct_month_label), axes = "all_x") +
  layers_theme +
  theme(strip.placement = "outside") +
  coord_equal()
  # layers_scale_coord

full_calendar |>
  ggplot(aes(x = ct_wday_label, y = ct_month_week)) +
  geom_tile(alpha = 0.2, color = "grey70") +
  facet_wrap(vars(ct_month_label), axes = "all_x") +
  geom_text(aes(label = ct_mday), nudge_y = 0.25) +
  scale_y_reverse() +
  ## extras
  geom_text(aes(label = emoji), nudge_y = -0.25) +
  scale_x_discrete(position = "top") +
  layers_theme +
  theme(strip.placement = "outside") +
  coord_equal()

# full_cal_list <-
#   demo_events(100) |>
#   mutate(event_duration = difftime(end, start, unit = "days")) |>
#   reframe_intervals(start, end) |>
#   group_by(unit_date) |>
#   arrange(unit_date) |>
#   remove_overlaps(unit_date, event_duration, "first") |>
#   fill_calendar(unit_date)

## base calendar
#' Title
#'
#' @param .events_long 
#' @param date_col 
#' @param cal_range 
#' @param unit 
#' @param locale 
#' @param week_start 
#' @param nrow 
#' @param ncol 
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' demo_events(20) |>
#'  mutate(event_duration = difftime(end, start, unit = "days")) |>
#'  reframe_intervals(start, end) |>
#'  group_by(unit_date) |>
#'  arrange(unit_date) |>
#'  remove_overlaps(unit_date, event_duration, "first") |>
#'  gg_facet_wrap_months(unit_date)
gg_facet_wrap_months <- function(.events_long, date_col, cal_range = NULL,
                        unit = "day", locale = NULL, week_start = NULL,
                        nrow = NULL, ncol = NULL){
  # date_col_str <- rlang::englue("{{date_col}}")
  # if(anyDuplicated(.events_long[[date_col_str]])){
  #   rlang::warn("More than one event per {.var {unit}}")
  # }
  
  cal_data <- fill_cal_list(.events_long, {{date_col}})
    
  cal_data$.merged_units |> 
    ggplot(mapping = aes(x = ct_wday_label, y = ct_month_week)) +
      geom_tile(alpha = 0.2, color = "grey70") +
      facet_wrap(vars(ct_month_label), axes = "all_x", nrow = nrow, ncol = ncol) +
      geom_text(aes(label = ct_mday), nudge_y = 0.25) +
      scale_y_reverse() +
      scale_x_discrete(position = "top")
}

full_cal_merged <- bind_rows(full_cal_list)

g_base <- ggplot(data = full_cal_merged,
       mapping = aes(x = ct_wday_label, y = ct_month_week)) +
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
  geom_text(data = full_cal_list$.event_units,
                   mapping = aes(label = emoji))


# ggsave("static.png")

# make_cal_base <- function(.data, unit_col)
