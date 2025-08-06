#' Label month facets
#' @description This function creates a custom \link[ggplot2]{labeller} for use 
#' in `ggplot2::facet_wrap()` to format dates (typically \link[tsibble]{yearmonth}) in a 
#' specified string format.
#' @param formatstring A character string specifying the format to be used
#  for formatting date values.
#'  
#'  - "%b" is "Jan", "Feb"
#'  - "%b %Y" is "Jan 2024", "Feb 2024"
#' @export
#' @return A labeller function suitable for the `labeller` argument in `gg_facet_wrap_months()`
#' @importFrom ggplot2 as_labeller
#' @importFrom tsibble yearmonth

label_yearmonth <- function(formatstring="%b") { 
  as_labeller(\(x) format(yearmonth(x), format = formatstring)) 
}