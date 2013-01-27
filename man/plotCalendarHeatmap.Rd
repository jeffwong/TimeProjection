\name{plotCalendarHeatmap}
\alias{plotCalendarHeatmap}
\title{Calendar Heatmap}
\usage{
  plotCalendarHeatmap(dates, values)
}
\arguments{
  \item{dates}{a vector of date objects}

  \item{values}{a numeric vector with same length as dates}
}
\description{
  Create a plot mimicing a calendar with a heatmap of
  values
}
\examples{
dates = timeSequence(from = '2012-01-01', to = '2012-12-31', by = 'day')
   plotCalendarHeatmap(as.Date(dates), 1:366)
}

