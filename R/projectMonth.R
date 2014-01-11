month.wdayDistribution = function(year, month) {
  t1 = as.Date(sprintf("%s-%s-01", year, month))
  t2 = t1 + 31
  t2.year = year(t2); t2.month = month(t2)
  t2 = as.Date(as.Date(sprintf("%s-%s-01", t2.year, t2.month)))
  dates = seq.Date(from = t1,
                   to = t2 - 1,
                   by = 1)
  dates.wday = wday(dates)
  table(dates.wday)
}

monthBegin = function(year, month) {
  as.Date(mapply(function(y,m) as.Date(sprintf("%s-%s-01", y, m)), year, month), origin='1970-01-01')
}

projectMonth = function(year, month) {
  projection = as.data.frame(t(mapply(function(y,m) month.wdayDistribution(y,m), year, month)))
  names(projection) = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  monthBegins = as.Date(mapply(function(y,m) as.Date(sprintf("%s-%s-01", y, m)), year, month), origin='1970-01-01')
  return (data.frame(month_begins_date=monthBegins, projection))
}
