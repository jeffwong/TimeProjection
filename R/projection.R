#' Time Projection
#'
#' Project dates to lower dimensional subspace.
#' Extracts components year, month, yday, mday, hour, minute, weekday
#' and bizday from a date object
#'
#' @param dates date or datetime objects
#' @param size either "narrow" or "wide".  If narrow, returns a data frame
#'    containing the projections as column variables using factors.
#'    If wide, returns a sparse matrix containing the projections as column
#'    variables using 0-1 variables
#' @param holidays argument to determine which days are considered holidays,
#'    affecting the business day projection.  By default uses holidayNYSE()
#'    provided by the timeDate package, or can be specified as a vector of
#'    strings representing dates in the yyyy-mm-dd format
#' @examples
#'    dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
#'    projectDate(dates)
#' @export
projectDate = function(dates, size = c("narrow", "wide"), holidays = holidayNYSE()) {
    year = factor(year(dates))
    month = factor(month(dates))
    yday = factor(yday(dates))
    mday = factor(mday(dates))
    hour = factor(hour(dates))
    minute = factor(minute(dates))
    weekday = factor(weekdays(dates))
    bizday = factor(is.Bizday(dates, holidays))
    raw = data.frame(year = year,
               month = month,
               yday = yday,
               mday = mday,
               hour = hour,
               minute = minute,
               weekday = weekday,
               bizday = bizday)
    raw.levels = apply(raw, 2, function(j) { nlevels(as.factor(j)) })
    size = match.arg(size)
    if (size == "narrow") return (subset(raw, select = which(raw.levels > 1)))
    if (size == "wide") {
        return (sparse.model.matrix(~ ., subset(raw, select = which(raw.levels > 1))))
    }
}

is.Bizday = function(x, holidays) 
{
    char.x = substr(as.character(x), 1, 10)
    char.h = substr(as.character(holidays), 1, 10)
    Weekday = as.integer(isWeekday(x, wday = 1:5))
    nonHoliday = as.integer(!(char.x %in% char.h))
    bizdays = as.logical(Weekday * nonHoliday)
    return (bizdays)
}

#' @method weekdays timeDate
#' @S3method weekdays timeDate
weekdays.timeDate = function(x, abbreviate=FALSE) {
    weekdays(as.Date(x), abbreviate)
}
