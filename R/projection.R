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
#'    strings representing dates in the yyyy-mm-dd format.  Use NULL if 
#'    holidays should not be considered when computing isBusinessDay
#' @param as.numeric logical only used when size = "narrow".  Returns the
#'    columns as numeric values instead of factors
#' @param ordered logical.  When reporting time projection as factors, should
#'    the factors be reported as ordered
#' @param prefix optional.  A character vector that is prepended to the column names
#'    of the time projection
#' @param drop logical.  If true, drop any column that only has 1 level or only
#'    1 unique element in it
#' @examples
#'    dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
#'    projectDate(as.Date(dates))
#' @export
projectDate = function(dates, size = c("narrow", "wide"),
                       holidays = holidayNYSE(year = unique(year(dates))),
                       as.numeric = F, ordered = T, prefix = "", drop = T) {
    size = match.arg(size)
    year = year(dates)
    month = month(dates)
    yday = yday(dates)
    mday = mday(dates)
    yweek = floor((yday - 1) / 7) + 1
    mweek = floor((mday - 1) / 7) + 1
    hour = hour(dates)
    minute = minute(dates)
    
    q1 = which(month %in% 1:3)
    q2 = which(month %in% 4:6)
    q3 = which(month %in% 7:9)
    q4 = which(month %in% 10:12)
    quarter = rep(0, length(dates))
    quarter[q1] = 1
    quarter[q2] = 2
    quarter[q3] = 3
    quarter[q4] = 4
    
    if (!as.numeric | size == "wide") {
        year = factor(year, ordered = ordered)
        quarter = factor(quarter, levels = 1:4, ordered = ordered)
        month = factor(month, levels = 1:12, ordered = ordered)
        yday = factor(yday, levels = 1:366, ordered = ordered)
        mday = factor(mday, levels = 1:31, ordered = ordered)
        yweek = factor(yweek, levels = 1:53, ordered = ordered)
        mweek = factor(mweek, levels = 1:5, ordered = ordered)
        hour = factor(hour, levels = 0:23, ordered = ordered)
        minute = factor(minute, levels = 0:59, ordered = ordered)
    }

    weekday_labels = weekdays(as.Date(timeSequence(from = "2012-01-01", to = "2012-01-07", by = "day")))
    weekday = factor(weekdays(dates), levels = weekday_labels, ordered = ordered)
    bizday = factor(is.Bizday(dates, holidays))
    season = factor(getSeason(dates), levels = c("Winter", "Spring", "Summer", "Fall"),
                    ordered = ordered)
    raw = data.frame(year = year,
                     quarter = quarter,
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
    if (prefix != "") colnames(raw) = paste(prefix, colnames(raw), sep=".")
    if (drop) {
        redundantCols = rep(F, ncol(raw))
        for (i in 1:ncol(raw)) {
            if (all(diff(as.numeric(raw[,i])) == 0)) redundantCols[i] = T
        }
        raw = raw[,!redundantCols]
    }
    if (size == "narrow") return (raw)
    if (size == "wide") {
        return (sparse.model.matrix(~ . -1, raw))
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

is.Bizday = function(x, holidays=NULL) 
{
    Weekday = as.integer(isWeekday(x, wday = 1:5))
    if (!is.null(holidays)) {
      char.x = substr(as.character(x), 1, 10)
      char.h = substr(as.character(holidays), 1, 10)
      nonHoliday = as.integer(!(char.x %in% char.h))
      bizdays = as.logical(Weekday * nonHoliday)
    }
    else {
      bizdays = as.logical(Weekday)
    }
    return (bizdays)
}

#' @method weekdays timeDate
#' @S3method weekdays timeDate
weekdays.timeDate = function(x, abbreviate=FALSE) {
    weekdays(as.Date(x), abbreviate)
}
