timeProjection = function(dates, metrics) {
    projection = projectDate(dates, "narrow")
    x = cbind(projection, metrics)
    return (x)
}

projectDate = function(dates, size = c("narrow", "wide"), ...) {
    year = factor(year(dates))
    month = factor(month(dates))
    yday = factor(yday(dates))
    mday = factor(mday(dates))
    hour = factor(hour(dates))
    minute = factor(minute(dates))
    weekday = factor(weekdays(dates))
    bizday = factor(is.Bizday(dates, ...))
    raw = data.frame(year = year,
               month = month,
               yday = yday,
               mday = mday,
               hour = hour,
               minute = minute,
               weekday = weekday,
               bizday = bizday)
    raw.levels = apply(raw, 2, function(j) { nlevels(as.factor(j)) })
    if (size[1] == "narrow") return (raw)
    if (size[1] == "wide") {
        return (sparse.model.matrix(~ ., subset(raw, select = which(raw.levels > 1))))
    }
}

is.Bizday = function (x, holidays = holidayNYSE(), wday = 1:5) 
{
    char.x = substr(as.character(x), 1, 10)
    char.h = substr(as.character(holidays), 1, 10)
    Weekday = as.integer(isWeekday(x, wday = wday))
    nonHoliday = as.integer(!(char.x %in% char.h))
    bizdays = as.logical(Weekday * nonHoliday)
    return (bizdays)
}
