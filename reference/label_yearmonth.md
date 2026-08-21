# Label month facets

This function creates a custom
[labeller](https://ggplot2.tidyverse.org/reference/labeller.html) for
use in
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
to format dates (typically
[yearmonth](https://tsibble.tidyverts.org/reference/year-month.html)) in
a specified string format.

## Usage

``` r
label_yearmonth(formatstring = "%b")
```

## Arguments

- formatstring:

  A character string specifying the format to be used

  - "%b" is "Jan", "Feb"

  - "%b %Y" is "Jan 2024", "Feb 2024"

## Value

A labeller function suitable for the `labeller` argument in
[`gg_facet_wrap_months()`](https://cynthiahqy.github.io/ggtilecal/reference/gg_facet_wrap_months.md)
