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