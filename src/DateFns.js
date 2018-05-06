var dateFns = require('date-fns')

exports.now = function () {
  return new Date()
}

exports.createDate = function(y) {
  return function (m) {
    return function (d) {
      return function () {
        return new Date(y, m, d)
      }
    }
  }
}

exports.unsafeCreateDate = function(v) {
  return function () {
    if (Array.isArray(v)) {
      return new (Date.bind.apply(Date, [null].concat(v)))
    }
    return new Date(v)
  }
}

exports.addDaysImpl = dateFns.addDays
exports.addHoursImpl = dateFns.addHours
exports.addISOYearsImpl = dateFns.addISOYears
exports.addMillisecondsImpl = dateFns.addMilliseconds
exports.addMinutesImpl = dateFns.addMinutes
exports.addMonthsImpl = dateFns.addMonths
exports.addQuartersImpl = dateFns.addQuarters
exports.addSecondsImpl = dateFns.addSeconds
exports.addWeeksImpl = dateFns.addWeeks
exports.addYearsImpl = dateFns.addYears
exports.areRangesOverlappingImpl = dateFns.areRangesOverlapping
exports.closestIndexToImpl = dateFns.closestIndexTo
exports.closestToImpl = dateFns.closestTo
exports.compareAscImpl = dateFns.compareAsc
exports.compareDescImpl = dateFns.compareDesc
exports.differenceInCalendarDaysImpl = dateFns.differenceInCalendarDays
exports.differenceInCalendarISOWeeksImpl = dateFns.differenceInCalendarISOWeeks
exports.differenceInCalendarISOYearsImpl = dateFns.differenceInCalendarISOYears
exports.differenceInCalendarMonthsImpl = dateFns.differenceInCalendarMonths
exports.differenceInCalendarQuartersImpl = dateFns.differenceInCalendarQuarters
exports.differenceInCalendarWeeksImpl = dateFns.differenceInCalendarWeeks
exports.differenceInCalendarYearsImpl = dateFns.differenceInCalendarYears
exports.differenceInDaysImpl = dateFns.differenceInDays
exports.differenceInHoursImpl = dateFns.differenceInHours
exports.differenceInISOYearsImpl = dateFns.differenceInISOYears
exports.differenceInMillisecondsImpl = dateFns.differenceInMilliseconds
exports.differenceInMinutesImpl = dateFns.differenceInMinutes
exports.differenceInMonthsImpl = dateFns.differenceInMonths
exports.differenceInQuartersImpl = dateFns.differenceInQuarters
exports.differenceInSecondsImpl = dateFns.differenceInSeconds
exports.differenceInWeeksImpl = dateFns.differenceInWeeks
exports.differenceInYearsImpl = dateFns.differenceInYears
exports.distanceInWordsImpl = dateFns.distanceInWords
exports.distanceInWordsStrictImpl = dateFns.distanceInWordsStrict
exports.distanceInWordsToNowImpl = dateFns.distanceInWordsToNow
exports.eachDayImpl = dateFns.eachDay
exports.endOfDayImpl = dateFns.endOfDay
exports.endOfHourImpl = dateFns.endOfHour
exports.endOfISOWeekImpl = dateFns.endOfISOWeek
exports.endOfISOYearImpl = dateFns.endOfISOYear
exports.endOfMinuteImpl = dateFns.endOfMinute
exports.endOfMonthImpl = dateFns.endOfMonth
exports.endOfQuarterImpl = dateFns.endOfQuarter
exports.endOfSecondImpl = dateFns.endOfSecond
exports.endOfTodayImpl = dateFns.endOfToday
exports.endOfTomorrowImpl = dateFns.endOfTomorrow
exports.endOfWeekImpl = dateFns.endOfWeek
exports.endOfYearImpl = dateFns.endOfYear
exports.endOfYesterdayImpl = dateFns.endOfYesterday
exports.formatImpl = dateFns.format
exports.getDateImpl = dateFns.getDate
exports.getDayImpl = dateFns.getDay
exports.getDayOfYearImpl = dateFns.getDayOfYear
exports.getDaysInMonthImpl = dateFns.getDaysInMonth
exports.getDaysInYearImpl = dateFns.getDaysInYear
exports.getHoursImpl = dateFns.getHours
exports.getISODayImpl = dateFns.getISODay
exports.getISOWeekImpl = dateFns.getISOWeek
exports.getISOWeeksInYearImpl = dateFns.getISOWeeksInYear
exports.getISOYearImpl = dateFns.getISOYear
exports.getMillisecondsImpl = dateFns.getMilliseconds
exports.getMinutesImpl = dateFns.getMinutes
exports.getMonthImpl = dateFns.getMonth
exports.getOverlappingDaysInRangesImpl = dateFns.getOverlappingDaysInRanges
exports.getQuarterImpl = dateFns.getQuarter
exports.getSecondsImpl = dateFns.getSeconds
exports.getTimeImpl = dateFns.getTime
exports.getYearImpl = dateFns.getYear
exports.isAfterImpl = dateFns.isAfter
exports.isBeforeImpl = dateFns.isBefore
exports.isDateImpl = dateFns.isDate
exports.isEqualImpl = dateFns.isEqual
exports.isFirstDayOfMonthImpl = dateFns.isFirstDayOfMonth
exports.isFridayImpl = dateFns.isFriday
exports.isFutureImpl = dateFns.isFuture
exports.isLastDayOfMonthImpl = dateFns.isLastDayOfMonth
exports.isLeapYearImpl = dateFns.isLeapYear
exports.isMondayImpl = dateFns.isMonday
exports.isPastImpl = dateFns.isPast
exports.isSameDayImpl = dateFns.isSameDay
exports.isSameHourImpl = dateFns.isSameHour
exports.isSameISOWeekImpl = dateFns.isSameISOWeek
exports.isSameISOYearImpl = dateFns.isSameISOYear
exports.isSameMinuteImpl = dateFns.isSameMinute
exports.isSameMonthImpl = dateFns.isSameMonth
exports.isSameQuarterImpl = dateFns.isSameQuarter
exports.isSameSecondImpl = dateFns.isSameSecond
exports.isSameWeekImpl = dateFns.isSameWeek
exports.isSameYearImpl = dateFns.isSameYear
exports.isSaturdayImpl = dateFns.isSaturday
exports.isSundayImpl = dateFns.isSunday
exports.isThisHourImpl = dateFns.isThisHour
exports.isThisISOWeekImpl = dateFns.isThisISOWeek
exports.isThisISOYearImpl = dateFns.isThisISOYear
exports.isThisMinuteImpl = dateFns.isThisMinute
exports.isThisMonthImpl = dateFns.isThisMonth
exports.isThisQuarterImpl = dateFns.isThisQuarter
exports.isThisSecondImpl = dateFns.isThisSecond
exports.isThisWeekImpl = dateFns.isThisWeek
exports.isThisYearImpl = dateFns.isThisYear
exports.isThursdayImpl = dateFns.isThursday
exports.isTodayImpl = dateFns.isToday
exports.isTomorrowImpl = dateFns.isTomorrow
exports.isTuesdayImpl = dateFns.isTuesday
exports.isValidImpl = dateFns.isValid
exports.isWednesdayImpl = dateFns.isWednesday
exports.isWeekendImpl = dateFns.isWeekend
exports.isWithinRangeImpl = dateFns.isWithinRange
exports.isYesterdayImpl = dateFns.isYesterday
exports.lastDayOfISOWeekImpl = dateFns.lastDayOfISOWeek
exports.lastDayOfISOYearImpl = dateFns.lastDayOfISOYear
exports.lastDayOfMonthImpl = dateFns.lastDayOfMonth
exports.lastDayOfQuarterImpl = dateFns.lastDayOfQuarter
exports.lastDayOfWeekImpl = dateFns.lastDayOfWeek
exports.lastDayOfYearImpl = dateFns.lastDayOfYear
exports.maxImpl = function(dates) {
  return dateFns.max.apply(null, dates)
}
exports.minImpl = function(dates) {
  return dateFns.min.apply(null, dates)
}
exports.parseImpl = dateFns.parse
exports.setDateImpl = dateFns.setDate
exports.setDayImpl = dateFns.setDay
exports.setDayOfYearImpl = dateFns.setDayOfYear
exports.setHoursImpl = dateFns.setHours
exports.setISODayImpl = dateFns.setISODay
exports.setISOWeekImpl = dateFns.setISOWeek
exports.setISOYearImpl = dateFns.setISOYear
exports.setMillisecondsImpl = dateFns.setMilliseconds
exports.setMinutesImpl = dateFns.setMinutes
exports.setMonthImpl = dateFns.setMonth
exports.setQuarterImpl = dateFns.setQuarter
exports.setSecondsImpl = dateFns.setSeconds
exports.setYearImpl = dateFns.setYear
exports.startOfDayImpl = dateFns.startOfDay
exports.startOfHourImpl = dateFns.startOfHour
exports.startOfISOWeekImpl = dateFns.startOfISOWeek
exports.startOfISOYearImpl = dateFns.startOfISOYear
exports.startOfMinuteImpl = dateFns.startOfMinute
exports.startOfMonthImpl = dateFns.startOfMonth
exports.startOfQuarterImpl = dateFns.startOfQuarter
exports.startOfSecondImpl = dateFns.startOfSecond
exports.startOfTodayImpl = dateFns.startOfToday
exports.startOfTomorrowImpl = dateFns.startOfTomorrow
exports.startOfWeekImpl = dateFns.startOfWeek
exports.startOfYearImpl = dateFns.startOfYear
exports.startOfYesterdayImpl = dateFns.startOfYesterday
exports.subDaysImpl = dateFns.subDays
exports.subHoursImpl = dateFns.subHours
exports.subISOYearsImpl = dateFns.subISOYears
exports.subMillisecondsImpl = dateFns.subMilliseconds
exports.subMinutesImpl = dateFns.subMinutes
exports.subMonthsImpl = dateFns.subMonths
exports.subQuartersImpl = dateFns.subQuarters
exports.subSecondsImpl = dateFns.subSeconds
exports.subWeeksImpl = dateFns.subWeeks
exports.subYearsImpl = dateFns.subYears