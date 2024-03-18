requireNamespace("ggplot2", quietly = TRUE)

layers_scale_coord <- list(
  ggplot2::scale_y_reverse(),
  ggplot2::coord_fixed(expand = TRUE)
)

#' @importFrom ggplot2 element_blank element_rect element_text
layers_theme <- list(
  ggplot2::labs(y = NULL, x = NULL),
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
  ## extras
  geom_text(aes(label = emoji), nudge_y = -0.25) +
  scale_x_discrete(position = "top") +
  layers_theme +
  theme(strip.placement = "outside") +
  coord_equal()

# ggsave("static.png")

# make_cal_base <- function(.data, unit_col)
