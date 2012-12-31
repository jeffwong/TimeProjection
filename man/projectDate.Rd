\name{projectDate}
\alias{projectDate}
\title{Time Projection}
\usage{
  projectDate(dates, size = c("narrow", "wide"))
}
\arguments{
  \item{dates}{date or datetime objects}

  \item{size}{either "narrow" or "wide.  If narrow, returns
  a data frame containing the projections as column
  variables using factors.  If wide, returns a sparse
  matrix containing the projections as column variables
  using 0-1 variables}
}
\description{
  Project dates to lower dimensional subspace
}

