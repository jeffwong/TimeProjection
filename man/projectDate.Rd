\name{projectDate}
\alias{projectDate}
\title{Time Projection}
\usage{
  projectDate(dates, size = c("narrow", "wide"),
    holidays = holidayNYSE(year = unique(year(dates))),
    drop = T)
}
\arguments{
  \item{dates}{date or datetime objects}

  \item{size}{either "narrow" or "wide".  If narrow,
  returns a data frame containing the projections as column
  variables using factors.  If wide, returns a sparse
  matrix containing the projections as column variables
  using 0-1 variables}

  \item{holidays}{argument to determine which days are
  considered holidays, affecting the business day
  projection.  By default uses holidayNYSE() provided by
  the timeDate package, or can be specified as a vector of
  strings representing dates in the yyyy-mm-dd format}

  \item{drop}{logical.  If true, drop any column that only
  has 1 level or only 1 unique element in it}
}
\description{
  Project dates to lower dimensional subspace. Extracts
  components year, month, yday, mday, hour, minute,
  weekday, bizday and season from a date object
}
\examples{
dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
   projectDate(as.Date(dates))
}

