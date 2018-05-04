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