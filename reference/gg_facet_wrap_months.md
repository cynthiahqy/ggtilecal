# Make Monthly Calendar Facets

Generates calendar with monthly facets by:

- Padding event list with any missing days via
  [`fill_missing_units()`](https://cynthiahqy.github.io/ggtilecal/reference/fill_missing_units.md)

- Calculating variables for calendar layout via
  [`calc_calendar_vars()`](https://cynthiahqy.github.io/ggtilecal/reference/calc_calendar_vars.md)

- Returning a ggplot object as per Details.

## Usage

``` r
gg_facet_wrap_months(
  .events_long,
  date_col,
  locale = NULL,
  week_start = NULL,
  nrow = NULL,
  ncol = NULL,
  labeller = label_yearmonth("%b"),
  .geom = list(geom_tile(color = "grey70", fill = "transparent"), geom_text(nudge_y =
    0.25)),
  .scale_coord = list(scale_y_reverse(), scale_x_discrete(position = "top"),
    coord_fixed(expand = TRUE)),
  .theme = list(theme_bw_tilecal()),
  .other = list()
)
```

## Arguments

- .events_long:

  long format calendar event data

- date_col:

  column containing calendar unit dates

- locale:

  locale to use for day names. Default to current locale.

- week_start:

  day on which week starts following ISO conventions: 1 means Monday and
  7 means Sunday (default). When `label = FALSE` and `week_start = 7`,
  the number returned for Sunday is 1, for Monday is 2, etc. When
  `label = TRUE`, the returned value is a factor with the first level
  being the week start (e.g. Sunday if `week_start = 7`). You can set
  `lubridate.week.start` option to control this parameter globally.

- nrow, ncol:

  Number of rows and columns.

- labeller:

  A facet labeling function, for the calendar month titles, defaults to
  [`label_yearmonth()`](https://cynthiahqy.github.io/ggtilecal/reference/label_yearmonth.md)

- .geom, .scale_coord, .theme, .other:

  Customisable lists of ggplot2 components to add to the plot. An empty
  [`list()`](https://rdrr.io/r/base/list.html) leaves the plot
  unmodified.

## Value

ggplot

## Details

Returns a ggplot with the following fixed components using calculated
layout variables:

- [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) mapping:

  - `x` is day of week,

  - `y` is week in month,

  - `label` is day of month

- [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  by month

- [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) to
  remove axis labels for calculated layout variables

and default customisable components:

- [`geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  to label each day which inherit calculated variables

- [`scale_y_reverse()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  to order day in month correctly

- [`scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html)
  to position weekday labels

- [`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
  to square each tile

- [`theme_bw_tilecal()`](https://cynthiahqy.github.io/ggtilecal/reference/theme_bw_tilecal.md)
  to apply sensible theme defaults

for the month facets:

- `nrow` the number of month facet rows

- or `ncol` the number of month facet columns

- a labeller function for the monthly facet strip labels, see
  [`label_yearmonth()`](https://cynthiahqy.github.io/ggtilecal/reference/label_yearmonth.md)

To modify components alter the `.geom` and `.scale_coord`, which inherit
the calculate layout mapping by default (via the ggplot2 `inherit.aes`
argument).

To additional components use the ggplot `+` function as normal, or pass
components to the `.other` argument. This can be used to add interactive
geoms (e.g. from `ggiraph`)

To modify the theme, use the ggplot `+` function as normal, or add
additional elements to the list in `.theme`.

To remove any of the optional components, set the argument to any empty
[`list()`](https://rdrr.io/r/base/list.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(ggplot2)
demo_events_gpt |>
  reframe_events(startDate, endDate) |>
  group_by(unit_date) |>
  slice_min(order_by = duration) |>
  gg_facet_wrap_months(unit_date, 
                       labeller=label_yearmonth("%b %Y")) +
  geom_text(aes(label = event_emoji), nudge_y = -0.25, na.rm = TRUE) } # } 
```
