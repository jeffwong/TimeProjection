\name{projectDate}
\alias{projectDate}
\title{Time Projection}
\usage{
  projectDate(dates, size = c("narrow", "wide"),
    holidays = holidayNYSE())
}
\arguments{
  \item{dates}{date or datetime objects}

  \item{size}{either "narrow" or "wide".  If narrow,
  returns a data frame containing the projections as column
  variables using factors.  If wide, returns a sparse
  matrix containing the projections as column variables
  using 0-1 variables}

  \item{holidays}{argument to determine which days are
  considered holidays.  By default uses holidayNYSE
  provided by the timeDate package}
}
\description{
  Project dates to lower dimensional subspace. Extracts
  components year, month, yday, mday, hour, minute, weekday
  and bizday from a date object
}
\examples{
dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
   projectDate(dates)
}

