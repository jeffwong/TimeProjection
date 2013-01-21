#' Time Projection
#'
#' Project dates to lower dimensional subspace.
#' Extracts components year, month, yday, mday, hour, minute, weekday,
#' bizday and season from a date object
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
#' @param drop logical.  If true, drop any column that only has 1 level or only
#'    1 unique element in it
#' @examples
#'    dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
#'    projectDate(dates)
#' @export
projectDate = function(dates, size = c("narrow", "wide"),
                       holidays = holidayNYSE(year = unique(year(dates))),
                       drop = T) {
    dates = dates[order(dates)]

    year = factor(year(dates))
    month = factor(month(dates))

    yday.numeric = yday(dates)
    mday.numeric = mday(dates)

    yweek = factor((yday.numeric - 1) / 7)
    yday = factor(yday.numeric)
    
    mweek = factor((mday.numeric - 1) / 7)
    mday = factor(mday.numeric)

    hour = factor(hour(dates))
    minute = factor(minute(dates))
    weekday = factor(weekdays(dates), levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday"))
    bizday = factor(is.Bizday(dates, holidays))
    season = factor(getSeason(dates), levels = c("Winter", "Spring", "Summer", "Fall"))
    raw = data.frame(year = year,
                     month = month,
                     yweek = yweek,
                     mweek = mweek,
                     yday = yday,
                     mday = mday,
                     hour = hour,
                     minute = minute,
                     weekday = weekday,
                     bizday = bizday,
                     season = season)
    if (drop) {
        redundantCols = apply(raw, 2, function(j) { nlevels(j) == 1 | all(diff(as.numeric(j)) == 0) })
        raw = raw[,!redundantCols]
    }
    size = match.arg(size)
    if (size == "narrow") return (raw)
    if (size == "wide") {
        return (sparse.model.matrix(~ ., raw))
    }
}

# Source: http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
getSeason <- function(dates) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(dates, format="2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
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
