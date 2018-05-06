module Test.Main where

import DateFns

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Traversable (sequence, traverse, traverse_)
import DateFns.Locale as L
import DateFns.Types (DateFnsEff)
import Prelude (class Show, Unit, bind, discard, show, ($), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)
myLog :: ∀ a b. Show a => String -> a -> Eff ( console :: CONSOLE | b) Unit
myLog name v = logShow $ (name <> ": " <> (show v))

main :: ∀ e. Eff (DateFnsEff (console :: CONSOLE | e)) Unit
main = do
  d <- now
  -- addHours d 10
  logShow d
  do
    date <- unsafeCreateDate [2014, 8, 1]
    addDays date 10 >>= (myLog "addDays")
  do
    date <- unsafeCreateDate [2014, 6, 10, 23, 0]
    addHours date 2 >>= (myLog "addHours")
  do
    date <- unsafeCreateDate [2010, 6, 2]
    addISOYears date 2 >>= (myLog "addISOYears")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 45, 30, 0]
    addMilliseconds date 750 >>= (myLog "addMilliseconds")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 0]
    addMinutes date 30 >>= (myLog "addMinutes")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    addMonths date 5 >>= (myLog "addMonths")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    addQuarters date 1 >>= (myLog "addQuarters")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 45, 0]
    addSeconds date 30 >>= (myLog "addSeconds")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    addWeeks date 4 >>= (myLog "addWeeks")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    addYears date 5 >>= (myLog "addYears")
  do
    initialRangeStartDate  <- unsafeCreateDate [2014, 0, 10]
    initialRangeEndDate    <- unsafeCreateDate [2014, 0, 20]
    comparedRangeStartDate <- unsafeCreateDate [2014, 0, 17]
    comparedRangeEndDate   <- unsafeCreateDate [2014, 0, 21]
    areRangesOverlapping initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate >>= (myLog "areRangesOverlapping")
  do
    dateToCompare <- unsafeCreateDate [2015, 8, 6]
    datesArray    <- sequence [unsafeCreateDate [2015, 0, 1], unsafeCreateDate [2016, 0, 1], unsafeCreateDate [2017, 0, 1]]
    closestIndexTo dateToCompare datesArray >>= (myLog "closestIndexTo")
  do
    dateToCompare <- unsafeCreateDate [2015, 8, 6]
    datesArray    <- sequence [unsafeCreateDate [2000, 0, 1], unsafeCreateDate [2030, 0, 1]]
    closestTo dateToCompare datesArray >>= (myLog "closestTo")
  do
    dateLeft  <- unsafeCreateDate [1987, 1, 11]
    dateRight <- unsafeCreateDate [1989, 6, 10]
    compareAsc dateLeft dateRight >>= (myLog "compareAsc")
  do
    dateLeft  <- unsafeCreateDate [1987, 1, 11]
    dateRight <- unsafeCreateDate [1989, 6, 10]
    compareDesc dateLeft dateRight >>= (myLog "compareDesc")
  do
    dateLeft  <- unsafeCreateDate [2013, 6, 2]
    dateRight <- unsafeCreateDate [2012, 6, 2]
    differenceInCalendarDays dateLeft dateRight >>= (myLog "differenceInCalendarDays")
  do
    dateLeft  <- unsafeCreateDate [2014, 6, 21]
    dateRight <- unsafeCreateDate [2014, 6, 6]
    differenceInCalendarISOWeeks dateLeft dateRight >>= (myLog "differenceInCalendarISOWeeks")
  do
    dateLeft  <- unsafeCreateDate [2012, 0, 1]
    dateRight <- unsafeCreateDate [2000, 0, 1]
    differenceInCalendarISOYears dateLeft dateRight >>= (myLog "differenceInCalendarISOYears")
  do
    dateLeft  <- unsafeCreateDate [2014, 8, 1]
    dateRight <- unsafeCreateDate [2014, 0, 1]
    differenceInCalendarMonths dateLeft dateRight >>= (myLog "differenceInCalendarMonths")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2]
    dateRight <- unsafeCreateDate [2013, 11, 31]
    differenceInCalendarQuarters dateLeft dateRight >>= (myLog "differenceInCalendarQuarters")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 20]
    dateRight <- unsafeCreateDate [2014, 6, 5]
    differenceInCalendarWeeks dateLeft dateRight {weekStartsOn: 1}  >>= (myLog "differenceInCalendarWeeks")

  do
    dateLeft  <- unsafeCreateDate [2015, 1, 11]
    dateRight <- unsafeCreateDate [2013, 11, 31]
    differenceInCalendarYears dateLeft dateRight >>= (myLog "differenceInCalendarYears")

  do
    dateLeft  <- unsafeCreateDate [2012, 6, 2]
    dateRight <- unsafeCreateDate [2011, 6, 2]
    differenceInDays dateLeft dateRight >>= (myLog "differenceInDays")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2, 19, 0]
    dateRight <- unsafeCreateDate [2014, 6, 2, 6, 50]
    differenceInHours dateLeft dateRight >>= (myLog "differenceInHours")

  do
    dateLeft  <- unsafeCreateDate [2012, 0, 1]
    dateRight <- unsafeCreateDate [2010, 0, 1]
    differenceInISOYears dateLeft dateRight >>= (myLog "differenceInISOYears")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2, 12, 30, 21, 700]
    dateRight <- unsafeCreateDate [2014, 6, 2, 12, 30, 20, 600]
    differenceInMilliseconds dateLeft dateRight >>= (myLog "differenceInMilliseconds")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2, 12, 20, 0]
    dateRight <- unsafeCreateDate [2014, 6, 2, 12, 7, 59]
    differenceInMinutes dateLeft dateRight >>= (myLog "differenceInMinutes")

  do
    dateLeft  <- unsafeCreateDate [2014, 8, 1]
    dateRight <- unsafeCreateDate [2014, 0, 31]
    differenceInMonths dateLeft dateRight >>= (myLog "differenceInMonths")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2]
    dateRight <- unsafeCreateDate [2013, 11, 31]
    differenceInQuarters dateLeft dateRight >>= (myLog "differenceInQuarters")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 2, 12, 30, 20, 0]
    dateRight <- unsafeCreateDate [2014, 6, 2, 12, 30, 7, 999]
    differenceInSeconds dateLeft dateRight >>= (myLog "differenceInSeconds")

  do
    dateLeft  <- unsafeCreateDate [2014, 6, 20]
    dateRight <- unsafeCreateDate [2014, 6, 5]
    differenceInWeeks dateLeft dateRight >>= (myLog "differenceInWeeks")

  do
    dateLeft  <- unsafeCreateDate [2015, 1, 11]
    dateRight <- unsafeCreateDate [2013, 11, 31]
    differenceInYears dateLeft dateRight >>= (myLog "differenceInYears")

  do
    dateLeft  <- unsafeCreateDate [2015, 0, 1, 0, 0, 15]
    dateRight <- unsafeCreateDate [2015, 0, 1, 0, 0, 0]
    distanceInWords dateLeft dateRight {includeSeconds: true} >>= (myLog "distanceInWords")

  do
    dateLeft  <- unsafeCreateDate [2015, 0, 28]
    dateRight <- unsafeCreateDate [2015, 0, 1]
    distanceInWordsStrict dateLeft dateRight {unit: "M", partialMethod: "ceil"} >>= (myLog "distanceInWordsStrict")

  do
    date  <- unsafeCreateDate [2015, 0, 1, 0, 0, 15]
    distanceInWordsToNow date {includeSeconds: true} >>= (myLog "distanceInWordsToNow")

  do
    startDate  <- unsafeCreateDate [2014, 9, 6]
    endDate  <- unsafeCreateDate [2014, 9, 10]
    result <- eachDay startDate endDate 1
    logShow "--- eachDay start ---"
    traverse_ logShow result
    logShow "--- eachDay end ---"

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfDay date >>= (myLog "endOfDay")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55]
    endOfHour date >>= (myLog "endOfHour")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfISOWeek date >>= (myLog "endOfISOWeek")

  do
    date <- unsafeCreateDate [2005, 6, 2]
    endOfISOYear date >>= (myLog "endOfISOYear")

  do
    date <- unsafeCreateDate [2014, 11, 1, 22, 15, 45, 400]
    endOfMinute date >>= (myLog "endOfMinute")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfMonth date >>= (myLog "endOfMonth")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfQuarter date >>= (myLog "endOfQuarter")

  do
    date <- unsafeCreateDate [2014, 11, 1, 22, 15, 45, 400]
    endOfSecond date >>= (myLog "endOfSecond")

  do
    endOfToday >>= (myLog "endOfToday")

  do
    endOfTomorrow >>= (myLog "endOfTomorrow")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfWeek date {weekStartsOn: 1} >>= (myLog "endOfWeek")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    endOfYear date >>= (myLog "endOfYear")

  do
    endOfYesterday >>= (myLog "endOfYesterday")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    format date "Do [de] MMMM YYYY" {locale: L.eo} >>= (myLog "format")

  do
    date <- unsafeCreateDate [2012, 1, 29]
    getDate date >>= (myLog "getDate")

  do
    date <- unsafeCreateDate [2012, 1, 29]
    getDay date >>= (myLog "getDay")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    getDayOfYear date >>= (myLog "getDayOfYear")

  do
    date <- unsafeCreateDate [2000, 1]
    getDaysInMonth date >>= (myLog "getDaysInMonth")

  do
    date <- unsafeCreateDate [2012, 0, 1]
    getDaysInYear date >>= (myLog "getDaysInYear")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45]
    getHours date >>= (myLog "getHours")

  do
    date <- unsafeCreateDate [2012, 1, 26]
    getISODay date >>= (myLog "getISODay")

  do
    date <- unsafeCreateDate [2005, 0, 2]
    getISOWeek date >>= (myLog "getISOWeek")

  do
    date <- unsafeCreateDate [2015, 1, 11]
    getISOWeeksInYear date >>= (myLog "getISOWeeksInYear")

  do
    date <- unsafeCreateDate [2005, 0, 2]
    getISOYear date >>= (myLog "getISOYear")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45, 5, 123]
    getMilliseconds date >>= (myLog "getMilliseconds")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45, 5]
    getMinutes date >>= (myLog "getMinutes")

  do
    date <- unsafeCreateDate [2012, 1, 29]
    getMonth date >>= (myLog "getMonth")

  do
    initialRangeStartDate  <- unsafeCreateDate [2014, 0, 10]
    initialRangeEndDate    <- unsafeCreateDate [2014, 0, 20]
    comparedRangeStartDate <- unsafeCreateDate [2014, 0, 17]
    comparedRangeEndDate   <- unsafeCreateDate [2014, 0, 21]
    getOverlappingDaysInRanges initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate
      >>= (myLog "getOverlappingDaysInRanges")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    getQuarter date >>= (myLog "getQuarter")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45, 5, 123]
    getSeconds date >>= (myLog "getSeconds")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45, 5, 123]
    getTime date >>= (myLog "getTime")

  do
    date <- unsafeCreateDate [2012, 1, 29, 11, 45, 5, 123]
    getYear date >>= (myLog "getYear")

  do
    date <- unsafeCreateDate [1989, 6, 10]
    dateToCompare <- unsafeCreateDate [1987, 1, 11]
    isAfter date dateToCompare >>= (myLog "isAfter")

  do
    date <- unsafeCreateDate [1989, 6, 10]
    dateToCompare <- unsafeCreateDate [1987, 1, 11]
    isBefore date dateToCompare >>= (myLog "isBefore")

  do
    let argument = "mayonnaise"
    isDate argument >>= (myLog "isDate")

  do
    dateLeft <- unsafeCreateDate [2014, 6, 2, 6, 30, 45, 0]
    dateRight <- unsafeCreateDate [2014, 6, 2, 6, 30, 45, 500]
    isEqual dateLeft dateRight >>= (myLog "isEqual")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    isFirstDayOfMonth date >>= (myLog "isFirstDayOfMonth")

  do
    date <- unsafeCreateDate [2014, 8, 26]
    isFriday date >>= (myLog "isFriday")

  do
    date <- unsafeCreateDate [2014, 11, 31]
    isFuture date >>= (myLog "isFuture")

  do
    date <- unsafeCreateDate [2014, 1, 28]
    isLastDayOfMonth date >>= (myLog "isLastDayOfMonth")

  do
    date <- unsafeCreateDate [2012, 8, 1]
    isLeapYear date >>= (myLog "isLeapYear")

  do
    date <- unsafeCreateDate [2014, 8, 22]
    isMonday date >>= (myLog "isMonday")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    isPast date >>= (myLog "isPast")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 4, 6, 0]
    dateRight <- unsafeCreateDate [2014, 8, 4, 18, 0]
    isSameDay dateLeft dateRight >>= (myLog "isSameDay")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 4, 6, 0]
    dateRight <- unsafeCreateDate [2014, 8, 4, 18, 0]
    isSameHour dateLeft dateRight >>= (myLog "isSameHour")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 1]
    dateRight <- unsafeCreateDate [2014, 8, 7]
    isSameISOWeek dateLeft dateRight >>= (myLog "isSameISOWeek")

  do
    dateLeft <- unsafeCreateDate [2003, 11, 29]
    dateRight <- unsafeCreateDate [2005, 0, 2]
    isSameISOYear dateLeft dateRight >>= (myLog "isSameISOYear")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 4, 6, 30]
    dateRight <- unsafeCreateDate [2014, 8, 4, 6, 30, 15]
    isSameMinute dateLeft dateRight >>= (myLog "isSameMinute")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 2]
    dateRight <- unsafeCreateDate [2014, 8, 25]
    isSameMonth dateLeft dateRight >>= (myLog "isSameMonth")

  do
    dateLeft <- unsafeCreateDate [2014, 0, 1]
    dateRight <- unsafeCreateDate [2014, 2, 8]
    isSameQuarter dateLeft dateRight >>= (myLog "isSameQuarter")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 4, 6, 30, 15]
    dateRight <- unsafeCreateDate [2014, 8, 4, 6, 30, 15, 500]
    isSameSecond dateLeft dateRight >>= (myLog "isSameSecond")

  do
    dateLeft <- unsafeCreateDate [2014, 7, 31]
    dateRight <- unsafeCreateDate [2014, 8, 4]
    isSameWeek dateLeft dateRight {weekStartsOn: 1} >>= (myLog "isSameWeek")

  do
    dateLeft <- unsafeCreateDate [2014, 8, 2]
    dateRight <- unsafeCreateDate [2014, 8, 25]
    isSameYear dateLeft dateRight >>= (myLog "isSameYear")

  do
    date <- unsafeCreateDate [2014, 8, 27]
    isSaturday date >>= (myLog "isSaturday")

  do
    date <- unsafeCreateDate [2014, 8, 21]
    isSunday date >>= (myLog "isSunday")

  do
    date <- unsafeCreateDate [2014, 8, 25, 18]
    isThisHour date >>= (myLog "isThisHour")

  do
    date <- unsafeCreateDate [2014, 8, 22]
    isThisISOWeek date >>= (myLog "isThisISOWeek")

  do
    date <- unsafeCreateDate [2013, 11, 30]
    isThisISOYear date >>= (myLog "isThisISOYear")

  do
    date <- unsafeCreateDate [2014, 8, 25, 18, 30]
    isThisMinute date >>= (myLog "isThisMinute")

  do
    date <- unsafeCreateDate [2014, 8, 15]
    isThisMonth date >>= (myLog "isThisMonth")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    isThisQuarter date >>= (myLog "isThisQuarter")

  do
    date <- unsafeCreateDate [2014, 8, 25, 18, 30, 15]
    isThisSecond date >>= (myLog "isThisSecond")

  do
    date <- unsafeCreateDate [2014, 8, 21]
    isThisWeek date {weekStartsOn: 1} >>= (myLog "isThisWeek")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    isThisYear date >>= (myLog "isThisYear")

  do
    date <- unsafeCreateDate [2014, 8, 25]
    isThursday date >>= (myLog "isThursday")
  do
    date <- unsafeCreateDate [2014, 9, 6, 14, 0]
    isToday date >>= (myLog "isToday")
  do
    date <- unsafeCreateDate [2014, 9, 7, 14, 0]
    isTomorrow date >>= (myLog "isTomorrow")
  do
    date <- unsafeCreateDate [2014, 8, 23]
    isTuesday date >>= (myLog "isTuesday")
  do
    date <- unsafeCreateDate [2014, 1, 31]
    isValid date >>= (myLog "isValid")
  do
    date <- unsafeCreateDate [2014, 8, 24]
    isWednesday date >>= (myLog "isWednesday")
  do
    date <- unsafeCreateDate [2014, 9, 5]
    isWeekend date >>= (myLog "isWeekend")

  do
    date <- unsafeCreateDate [2014, 0, 10]
    dateStart <- unsafeCreateDate [2014, 0, 1]
    dateEnd <- unsafeCreateDate [2014, 0, 7]
    isWithinRange date dateStart dateEnd >>= (myLog "isWithinRange")

  do
    date <- unsafeCreateDate [2014, 9, 5, 14, 0]
    isYesterday date >>= (myLog "isYesterday")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    lastDayOfISOWeek date >>= (myLog "lastDayOfISOWeek")
  do
    date <- unsafeCreateDate [2005, 6, 2]
    lastDayOfISOYear date >>= (myLog "lastDayOfISOYear")
  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    lastDayOfMonth date >>= (myLog "lastDayOfMonth")
  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    lastDayOfQuarter date >>= (myLog "lastDayOfQuarter")
  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    lastDayOfWeek date {weekStartsOn: 1} >>= (myLog "lastDayOfWeek")
  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    lastDayOfYear date >>= (myLog "lastDayOfYear")

  do
    dates <- traverse (unsafeCreateDate) [[1989, 6, 10], [1987, 1, 11], [1995, 6, 2], [1990, 0, 1]]
    max dates >>= (myLog "max")

  do
    dates <- traverse (unsafeCreateDate) [[1989, 6, 10], [1987, 1, 11], [1995, 6, 2], [1990, 0, 1]]
    min dates >>= (myLog "min")

  do
    parse (unsafeCoerce "+02014101") {additionalDigits: 1} >>= (myLog "parse")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    setDate date 30 >>= (myLog "setDate")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    setDay date 0 {weekStartsOn: 1} >>= (myLog "setDay")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    setDayOfYear date 2 >>= (myLog "setDayOfYear")

  do
    date <- unsafeCreateDate [2014, 8, 1, 11, 30]
    setHours date 4 >>= (myLog "setHours")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    setISODay date 7 >>= (myLog "setISODay")

  do
    date <- unsafeCreateDate [2004, 7, 7]
    setISOWeek date 53 >>= (myLog "setISOWeek")

  do
    date <- unsafeCreateDate [2008, 11, 29]
    setISOYear date 2007 >>= (myLog "setISOYear")

  do
    date <- unsafeCreateDate [2014, 8, 1, 11, 30, 40, 500]
    setMilliseconds date 300 >>= (myLog "setMilliseconds")

  do
    date <- unsafeCreateDate [2014, 8, 1, 11, 30, 40]
    setMinutes date 45 >>= (myLog "setMinutes")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    setMonth date 1 >>= (myLog "setMonth")

  do
    date <- unsafeCreateDate [2014, 6, 2]
    setQuarter date 2 >>= (myLog "setQuarter")

  do
    date <- unsafeCreateDate [2014, 8, 1, 11, 30, 40]
    setSeconds date 45 >>= (myLog "setSeconds")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    setYear date 2013 >>= (myLog "setYear")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfDay date >>= (myLog "startOfDay")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55]
    startOfHour date >>= (myLog "startOfHour")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfISOWeek date >>= (myLog "startOfISOWeek")

  do
    date <- unsafeCreateDate [2005, 6, 2]
    startOfISOYear date >>= (myLog "startOfISOYear")

  do
    date <- unsafeCreateDate [2014, 11, 1, 22, 15, 45, 400]
    startOfMinute date >>= (myLog "startOfMinute")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfMonth date >>= (myLog "startOfMonth")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfQuarter date >>= (myLog "startOfQuarter")

  do
    date <- unsafeCreateDate [2014, 11, 1, 22, 15, 45, 400]
    startOfSecond date >>= (myLog "startOfSecond")

  do
    startOfToday >>= (myLog "startOfToday")

  do
    startOfTomorrow >>= (myLog "startOfTomorrow")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfWeek date {weekStartsOn: 1} >>= (myLog "startOfWeek")

  do
    date <- unsafeCreateDate [2014, 8, 2, 11, 55, 0]
    startOfYear date >>= (myLog "startOfYear")

  do
    startOfYesterday >>= (myLog "startOfYesterday")

  do
    date <- unsafeCreateDate [2014, 8, 1]
    subDays date 10 >>= (myLog "subDays")
  do
    date <- unsafeCreateDate [2014, 6, 11, 1, 0]
    subHours date 2 >>= (myLog "subHours")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    subISOYears date 5 >>= (myLog "subISOYears")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 45, 30, 0]
    subMilliseconds date 750 >>= (myLog "subMilliseconds")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 0]
    subMinutes date 30 >>= (myLog "subMinutes")
  do
    date <- unsafeCreateDate [2015, 1, 1]
    subMonths date 5 >>= (myLog "subMonths")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    subQuarters date 3 >>= (myLog "subQuarters")
  do
    date <- unsafeCreateDate [2014, 6, 10, 12, 45, 0]
    subSeconds date 30 >>= (myLog "subSeconds")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    subWeeks date 4 >>= (myLog "subWeeks")
  do
    date <- unsafeCreateDate [2014, 8, 1]
    subYears date 5 >>= (myLog "subYears")