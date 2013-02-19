TimeProjection
==============

[TimeProjection on CRAN](http://cran.r-project.org/web/packages/TimeProjection/index.html)

##Installing Dev Version

The latest dev version can be installed using the `devtools` package

```
install.packages('devtools')
require(devtools)
install_github("TimeProjection", "jeffwong")
```

Time projections take a date/datetime object, representing a specific instance in time, and projects it down to a lower dimensional subspace.  A lower dimensional subspace for time could be the subspace Day of Week, where we take an instance of time such as 2012-12-31 13:47 UTC-0800 and just project it down to "Monday".  A projection essentially grabs one major component of a date/datetime.  Useful components to extract are day of week, weekend vs weekday, holiday, month, day of month, and year.  

Using one single time variable, we can extract many more useful time variables.  This can be particularly useful for constructing regressions and decision trees.  The projections can be used as predictor variables in a linear regression in order to analyze seasonality and trends, and can also be used as predictor variables in a decision tree: for example, a time series spanning 10 years of monthly data may have different monthly affects that can be captured by extracting the month component and regressing on that factor.

TimeProjection creates a data frame that can be in one of two forms: *narrow* or *wide*.  Suppose we wanted to get the Day of Week projection as a data frame; using narrow, we would get one single column variable of factors, and each date object would fall under one of Sun-Sat.  This is a very compact form and useful for decision trees.  In the wide format, we would get 7 column variables using 0's or 1's, one column for each day of week.  The column is 1 if the date falls into that projected point - this is a much more useful form when constructing regressions.
