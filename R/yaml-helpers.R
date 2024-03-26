
event_fields <- c("startDate", "endDate", "details", "emoji", "link")

yaml_node_template <- function(fields, n, ...) {
  node <- vector("list", length(fields))
  # node <- as.list(rep(NA_character_, length(fields)))
  names(node) <- fields
  nodes <- rep(list(node), n)
  dots <- list(...)
  args <- c(list(x = nodes), dots)
  do.call(yaml::as.yaml, args)
}

parse_event_yaml <- function(string) {
  
}