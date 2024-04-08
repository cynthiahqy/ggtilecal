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
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
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
