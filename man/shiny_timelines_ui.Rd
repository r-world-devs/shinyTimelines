% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_timelines.R
\name{shiny_timelines_ui}
\alias{shiny_timelines_ui}
\title{Timeline component UI function}
\usage{
shiny_timelines_ui(
  id,
  pagination = 20,
  selected_visits = FALSE,
  followup_order = FALSE,
  lot_only = FALSE
)
}
\arguments{
\item{id}{Internal parameter for {shiny}.}

\item{pagination}{Default number of rows per page. Set NULL to turn off feature.}

\item{selected_visits}{Vector of "Treatment", "Lab" and "Vitals" value defining what visits should be initially shown.
Set NULL to turn off feature.}

\item{followup_order}{TRUE/FALSE value to turn on/off followup date filtering. Set NULL to turn off feature.}

\item{lot_only}{Show patients having line of therapy only. Set NULL to turn off feature.}
}
\description{
Timeline component UI function
}
