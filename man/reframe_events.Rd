% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reframe_events.R
\name{reframe_events}
\alias{reframe_events}
\title{Reframe event intervals to calendar units}
\usage{
reframe_events(
  .data,
  event_start,
  event_end,
  id_cols = NULL,
  dates_to = "unit_date",
  cal_unit = "day"
)
}
\arguments{
\item{.data}{A data frame or tibble containing event details}

\item{event_start}{column containing event start date}

\item{event_end}{column containing event end date}

\item{id_cols}{(optional) set of columns that uniquely identify each observation.
Ignored if \code{.data} is grouped (i.e. using \link[dplyr]{group_by}).
If \code{.data} is not grouped and \code{id_cols} is \code{NULL}, all columns except for
\code{event_start} and \code{event_end} are used.}

\item{dates_to}{string name for column to output calendar unit dates}

\item{cal_unit}{increment of calendar sequence passed to \code{by} argument in
\code{\link[base]{seq.Date}}}
}
\value{
See return value of \link[dplyr]{reframe}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
Convert rows of events to sequence of calendar units for plotting.
Duplicates all columns except for \code{event_start} and \code{event_date}.
Requires a unique id key for each event.

This is a thin wrapper around \code{dplyr::reframe()} that expands
each uniquely identified event in \code{.data} to a sequence of datetimes defined
by the \code{start}, \code{end} and the interval \code{cal_unit}.
}
\examples{
event <- data.frame(
  id = 1,
  start = as.Date("2024-01-05"),
  end = as.Date("2024-01-10")
)
reframe_events(event, start, end)
demo_events_gpt |>
  reframe_events(startDate, endDate)
}
