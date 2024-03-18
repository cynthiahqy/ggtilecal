# Posted issue: https://github.com/vubiostat/r-yaml/issues/141

library(yaml)
library(purrr)

## Working date handling
# ref: https://github.com/vubiostat/r-yaml/issues/18
# ref: https://github.com/r-lib/ymlthis/blob/15e54ad42ab7c1c10b96490c74e63999149afac1/R/yml.R#L184-L191
date_handler <- list(Date = function(x) as.character(x))
my_list <- list(date = as.Date(c("2012-10-10", "2014-03-28")))
yaml::as.yaml(my_list, handlers = date_handler)
## instead of a list, use tibble and df (with column.major = TRUE by default)
my_tbl <- tibble::tibble(date = as.Date(c("2012-10-10", "2014-03-28")))
yaml::as.yaml(my_tbl, handlers = date_handler) |> cat()
## but the date_handler doesn't apply with column.major = FALSE?
yaml::as.yaml(my_tbl, handlers = date_handler, column.major = FALSE) |> cat()

## Same issue with data.frames
my_df <- as.data.frame(my_tbl)
yaml::as.yaml(my_df, handlers = date_handler)
yaml::as.yaml(my_df, handlers = date_handler, column.major = FALSE)

## What if convert the tibble to list first?
# purrr::transpose also does the weird number conversion
purrr::transpose(my_tbl)
# but not if you do as.list() first
as.list(my_tbl) |>
    purrr::list_transpose(simplify = FALSE)

# what about logical handlers?
my_tbl$bool <- c(TRUE, FALSE)
verbatim_logical <- function(x) {
    result <- tolower(as.logical(x))
    class(result) <- "verbatim"
    return(result)
}
yml_handlers <- c(date_handler, list(logical = verbatim_logical))
# Logical handlers work fine either way, but dates depend on column.major
my_tbl |> yaml::as.yaml(handlers = yml_handlers, column.major = FALSE)
my_tbl |> yaml::as.yaml(handlers = yml_handlers, column.major = TRUE)
