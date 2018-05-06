module DateFns where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import DateFns.Types
import Prelude (($))


-- base methods
foreign import now :: ∀ e. Eff (DateFnsEff e) DateF
-- @TODO 能加入更多參數 小時-分-秒 等等
foreign import createDate :: ∀ e. Year -> Month -> Day -> Eff (DateFnsEff e) DateF

foreign import unsafeCreateDate :: ∀ a e. a -> Eff (DateFnsEff e) DateF

-- date-fns methods
foreign import addDaysImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addDays date amount = liftEff $ runEffFn2 addDaysImpl date amount

foreign import addHoursImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addHours date amount = liftEff $ runEffFn2 addHoursImpl date amount

foreign import addISOYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addISOYears date amount = liftEff $ runEffFn2 addISOYearsImpl date amount

foreign import addMillisecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMilliseconds date amount = liftEff $ runEffFn2 addMillisecondsImpl date amount

foreign import addMinutesImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMinutes date amount = liftEff $ runEffFn2 addMinutesImpl date amount

foreign import addMonthsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMonths date amount = liftEff $ runEffFn2 addMonthsImpl date amount

foreign import addQuartersImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addQuarters date amount = liftEff $ runEffFn2 addQuartersImpl date amount

foreign import addSecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addSeconds date amount = liftEff $ runEffFn2 addSecondsImpl date amount

foreign import addWeeksImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addWeeks date amount = liftEff $ runEffFn2 addWeeksImpl date amount

foreign import addYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

addYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addYears date amount = liftEff $ runEffFn2 addYearsImpl date amount

foreign import areRangesOverlappingImpl
  :: ∀ e
   . EffFn4 (DateFnsEff e) DateF DateF DateF DateF Boolean

areRangesOverlapping
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> DateF -> DateF -> m Boolean
areRangesOverlapping initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate =
  liftEff $ runEffFn4 areRangesOverlappingImpl initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate

foreign import closestIndexToImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF (Array DateF) Int

closestIndexTo
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> (Array DateF) -> m Int
closestIndexTo dateToCompare datesArray =
  liftEff $ runEffFn2 closestIndexToImpl dateToCompare datesArray

foreign import closestToImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF (Array DateF) DateF

closestTo
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> (Array DateF) -> m DateF
closestTo dateToCompare datesArray =
  liftEff $ runEffFn2 closestToImpl dateToCompare datesArray

foreign import compareAscImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF DateF

compareAsc
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m DateF
compareAsc dateLeft dateRight =
  liftEff $ runEffFn2 compareAscImpl dateLeft dateRight

foreign import compareDescImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF DateF

compareDesc
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m DateF
compareDesc dateLeft dateRight =
  liftEff $ runEffFn2 compareDescImpl dateLeft dateRight

foreign import differenceInCalendarDaysImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarDays dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarDaysImpl dateLeft dateRight

foreign import differenceInCalendarISOWeeksImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarISOWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarISOWeeks dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarISOWeeksImpl dateLeft dateRight

foreign import differenceInCalendarISOYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarISOYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarISOYearsImpl dateLeft dateRight

foreign import differenceInCalendarMonthsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarMonths dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarMonthsImpl dateLeft dateRight

foreign import differenceInCalendarQuartersImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarQuarters dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarQuartersImpl dateLeft dateRight

foreign import differenceInCalendarWeeksImpl
  :: ∀ e opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) Int

differenceInCalendarWeeks
  :: ∀ e m opts opts_
   . Union opts opts_ Options
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m Int
differenceInCalendarWeeks dateLeft dateRight options =
  liftEff $ runEffFn3 differenceInCalendarWeeksImpl dateLeft dateRight options

foreign import differenceInCalendarYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarYearsImpl dateLeft dateRight

foreign import differenceInDaysImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInDays dateLeft dateRight =
  liftEff $ runEffFn2 differenceInDaysImpl dateLeft dateRight

foreign import differenceInHoursImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInHours dateLeft dateRight =
  liftEff $ runEffFn2 differenceInHoursImpl dateLeft dateRight

foreign import differenceInISOYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInISOYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInISOYearsImpl dateLeft dateRight

foreign import differenceInMillisecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMilliseconds dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMillisecondsImpl dateLeft dateRight

foreign import differenceInMinutesImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMinutes dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMinutesImpl dateLeft dateRight

foreign import differenceInMonthsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMonths dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMonthsImpl dateLeft dateRight

foreign import differenceInQuartersImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInQuarters dateLeft dateRight =
  liftEff $ runEffFn2 differenceInQuartersImpl dateLeft dateRight

foreign import differenceInSecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInSeconds dateLeft dateRight =
  liftEff $ runEffFn2 differenceInSecondsImpl dateLeft dateRight

foreign import differenceInWeeksImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInWeeks dateLeft dateRight =
  liftEff $ runEffFn2 differenceInWeeksImpl dateLeft dateRight

foreign import differenceInYearsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInYearsImpl dateLeft dateRight

foreign import distanceInWordsImpl
  :: ∀ e opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) String

distanceInWords
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m String
distanceInWords dateLeft dateRight options =
  liftEff $ runEffFn3 distanceInWordsImpl dateLeft dateRight options

foreign import distanceInWordsStrictImpl
  :: ∀ e opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) String

distanceInWordsStrict
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsStrictOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m String
distanceInWordsStrict dateLeft dateRight options =
  liftEff $ runEffFn3 distanceInWordsStrictImpl dateLeft dateRight options

foreign import distanceInWordsToNowImpl
  :: ∀ e opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) String

distanceInWordsToNow
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsToNowOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m String
distanceInWordsToNow date options =
  liftEff $ runEffFn2 distanceInWordsToNowImpl date options


foreign import eachDayImpl
  :: ∀ e
   . EffFn3 (DateFnsEff e) DateF DateF Int (Array DateF)

eachDay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Int -> m (Array DateF)
eachDay startDate endDate step =
  liftEff $ runEffFn3 eachDayImpl startDate endDate step

foreign import endOfDayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfDay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfDay date =
  liftEff $ runEffFn1 endOfDayImpl date

foreign import endOfHourImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfHour
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfHour date =
  liftEff $ runEffFn1 endOfHourImpl date

foreign import endOfISOWeekImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfISOWeek date =
  liftEff $ runEffFn1 endOfISOWeekImpl date

foreign import endOfISOYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfISOYear date =
  liftEff $ runEffFn1 endOfISOYearImpl date

foreign import endOfMinuteImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfMinute
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfMinute date =
  liftEff $ runEffFn1 endOfMinuteImpl date

foreign import endOfMonthImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfMonth date =
  liftEff $ runEffFn1 endOfMonthImpl date

foreign import endOfQuarterImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfQuarter date =
  liftEff $ runEffFn1 endOfQuarterImpl date

foreign import endOfSecondImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

endOfSecond
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfSecond date =
  liftEff $ runEffFn1 endOfSecondImpl date

foreign import endOfTodayImpl
  :: ∀ e
   . Eff (DateFnsEff e) DateF

endOfToday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => m DateF
endOfToday =
  liftEff endOfTodayImpl

foreign import endOfTomorrowImpl
  :: ∀ e
   . Eff (DateFnsEff e) DateF

endOfTomorrow
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => m DateF
endOfTomorrow =
  liftEff endOfTomorrowImpl

foreign import endOfWeekImpl
  :: ∀ e opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) DateF

endOfWeek
  :: ∀ e m opts opts_
   . Union opts opts_ EndOfWeekOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m DateF
endOfWeek date options =
  liftEff $ runEffFn2 endOfWeekImpl date options

foreign import endOfYearImpl
  :: ∀ e opts
   . EffFn1 (DateFnsEff e) DateF DateF

endOfYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
endOfYear date =
  liftEff $ runEffFn1 endOfYearImpl date

foreign import endOfYesterdayImpl
  :: ∀ e
   . Eff (DateFnsEff e) DateF

endOfYesterday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => m DateF
endOfYesterday =
  liftEff endOfYesterdayImpl

foreign import formatImpl
  :: ∀ e opts
    . EffFn3 (DateFnsEff e) DateF String (Record opts) String

format
  :: ∀ e m opts opts_
   . Union opts opts_ FormatOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> String -> (Record opts) -> m String
format date formatTokens options
  = liftEff $ runEffFn3 formatImpl date formatTokens options

foreign import getDateImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Int

getDate
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getDate date =
  liftEff $ runEffFn1 getDateImpl date

foreign import getDayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getDay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getDay date =
  liftEff $ runEffFn1 getDayImpl date

foreign import getDayOfYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getDayOfYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getDayOfYear date =
  liftEff $ runEffFn1 getDayOfYearImpl date

foreign import getDaysInMonthImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getDaysInMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getDaysInMonth date =
  liftEff $ runEffFn1 getDaysInMonthImpl date

foreign import getDaysInYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getDaysInYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getDaysInYear date =
  liftEff $ runEffFn1 getDaysInYearImpl date

foreign import getHoursImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getHours date =
  liftEff $ runEffFn1 getHoursImpl date

foreign import getISODayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getISODay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getISODay date =
  liftEff $ runEffFn1 getISODayImpl date

foreign import getISOWeekImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getISOWeek date =
  liftEff $ runEffFn1 getISOWeekImpl date

foreign import getISOWeeksInYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getISOWeeksInYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getISOWeeksInYear date =
  liftEff $ runEffFn1 getISOWeeksInYearImpl date

foreign import getISOYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getISOYear date =
  liftEff $ runEffFn1 getISOYearImpl date

foreign import getMillisecondsImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getMilliseconds date =
  liftEff $ runEffFn1 getMillisecondsImpl date

foreign import getMinutesImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getMinutes date =
  liftEff $ runEffFn1 getMinutesImpl date

foreign import getMonthImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getMonth date =
  liftEff $ runEffFn1 getMonthImpl date

foreign import getOverlappingDaysInRangesImpl
  :: ∀ e
    . EffFn4 (DateFnsEff e) DateF DateF DateF DateF Int

getOverlappingDaysInRanges
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> DateF -> DateF -> m Int
getOverlappingDaysInRanges initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate =
  liftEff $ runEffFn4 getOverlappingDaysInRangesImpl initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate

foreign import getQuarterImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getQuarter date =
  liftEff $ runEffFn1 getQuarterImpl date

foreign import getSecondsImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getSeconds date =
  liftEff $ runEffFn1 getSecondsImpl date

foreign import getTimeImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getTime
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getTime date =
  liftEff $ runEffFn1 getTimeImpl date

foreign import getYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Int

getYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Int
getYear date =
  liftEff $ runEffFn1 getYearImpl date

foreign import isAfterImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isAfter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isAfter date dateToCompare =
  liftEff $ runEffFn2 isAfterImpl date dateToCompare

foreign import isBeforeImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isBefore
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isBefore date dateToCompare =
  liftEff $ runEffFn2 isBeforeImpl date dateToCompare

foreign import isDateImpl
  :: ∀ a e
    . EffFn1 (DateFnsEff e) a Boolean

isDate
  :: ∀ a e m
   . MonadEff (DateFnsEff e) m
  => a -> m Boolean
isDate argument =
  liftEff $ runEffFn1 isDateImpl argument

foreign import isEqualImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF DateF Boolean

isEqual
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isEqual dateLeft dateRight =
  liftEff $ runEffFn2 isEqualImpl dateLeft dateRight

foreign import isFirstDayOfMonthImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isFirstDayOfMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isFirstDayOfMonth date =
  liftEff $ runEffFn1 isFirstDayOfMonthImpl date

foreign import isFridayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isFriday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isFriday date =
  liftEff $ runEffFn1 isFridayImpl date

foreign import isFutureImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isFuture
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isFuture date =
  liftEff $ runEffFn1 isFutureImpl date

foreign import isLastDayOfMonthImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isLastDayOfMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isLastDayOfMonth date =
  liftEff $ runEffFn1 isLastDayOfMonthImpl date

foreign import isLeapYearImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isLeapYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isLeapYear date =
  liftEff $ runEffFn1 isLeapYearImpl date

foreign import isMondayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isMonday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isMonday date =
  liftEff $ runEffFn1 isMondayImpl date

foreign import isPastImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isPast
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isPast date =
  liftEff $ runEffFn1 isPastImpl date

foreign import isSameDayImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameDay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameDay dateLeft dateRight =
  liftEff $ runEffFn2 isSameDayImpl dateLeft dateRight

foreign import isSameHourImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameHour
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameHour dateLeft dateRight =
  liftEff $ runEffFn2 isSameHourImpl dateLeft dateRight

foreign import isSameISOWeekImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameISOWeek dateLeft dateRight =
  liftEff $ runEffFn2 isSameISOWeekImpl dateLeft dateRight

foreign import isSameISOYearImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameISOYear dateLeft dateRight =
  liftEff $ runEffFn2 isSameISOYearImpl dateLeft dateRight

foreign import isSameMinuteImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameMinute
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameMinute dateLeft dateRight =
  liftEff $ runEffFn2 isSameMinuteImpl dateLeft dateRight

foreign import isSameMonthImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameMonth dateLeft dateRight =
  liftEff $ runEffFn2 isSameMonthImpl dateLeft dateRight

foreign import isSameQuarterImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameQuarter dateLeft dateRight =
  liftEff $ runEffFn2 isSameQuarterImpl dateLeft dateRight

foreign import isSameSecondImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameSecond
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameSecond dateLeft dateRight =
  liftEff $ runEffFn2 isSameSecondImpl dateLeft dateRight

foreign import isSameWeekImpl
  :: ∀ e opts
    . EffFn3 (DateFnsEff e) DateF DateF (Record opts) Boolean

isSameWeek
  :: ∀ e m opts opts_
   . Union opts opts_ IsSameWeekOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m Boolean
isSameWeek dateLeft dateRight options =
  liftEff $ runEffFn3 isSameWeekImpl dateLeft dateRight options

foreign import isSameYearImpl
  :: ∀ e
    . EffFn2 (DateFnsEff e) DateF DateF Boolean

isSameYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Boolean
isSameYear dateLeft dateRight =
  liftEff $ runEffFn2 isSameYearImpl dateLeft dateRight

foreign import isSaturdayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isSaturday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isSaturday date =
  liftEff $ runEffFn1 isSaturdayImpl date

foreign import isSundayImpl
  :: ∀ e
    . EffFn1 (DateFnsEff e) DateF Boolean

isSunday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isSunday date =
  liftEff $ runEffFn1 isSundayImpl date

foreign import isThisHourImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisHour
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisHour date =
  liftEff $ runEffFn1 isThisHourImpl date

foreign import isThisISOWeekImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisISOWeek date =
  liftEff $ runEffFn1 isThisISOWeekImpl date

foreign import isThisISOYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisISOYear date =
  liftEff $ runEffFn1 isThisISOYearImpl date

foreign import isThisMinuteImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisMinute
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisMinute date =
  liftEff $ runEffFn1 isThisMinuteImpl date

foreign import isThisMonthImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisMonth date =
  liftEff $ runEffFn1 isThisMonthImpl date

foreign import isThisQuarterImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisQuarter date =
  liftEff $ runEffFn1 isThisQuarterImpl date

foreign import isThisSecondImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisSecond
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisSecond date =
  liftEff $ runEffFn1 isThisSecondImpl date

foreign import isThisWeekImpl
  :: ∀ e opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) Boolean

isThisWeek
  :: ∀ e m opts opts_
   . Union opts opts_ IsThisWeekOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m Boolean
isThisWeek date options =
  liftEff $ runEffFn2 isThisWeekImpl date options

foreign import isThisYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThisYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThisYear date =
  liftEff $ runEffFn1 isThisYearImpl date

foreign import isThursdayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isThursday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isThursday date =
  liftEff $ runEffFn1 isThursdayImpl date

foreign import isTodayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isToday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isToday date =
  liftEff $ runEffFn1 isTodayImpl date

foreign import isTomorrowImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isTomorrow
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isTomorrow date =
  liftEff $ runEffFn1 isTomorrowImpl date

foreign import isTuesdayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isTuesday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isTuesday date =
  liftEff $ runEffFn1 isTuesdayImpl date

foreign import isValidImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isValid
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isValid date =
  liftEff $ runEffFn1 isValidImpl date

foreign import isWednesdayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isWednesday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isWednesday date =
  liftEff $ runEffFn1 isWednesdayImpl date

foreign import isWeekendImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isWeekend
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isWeekend date =
  liftEff $ runEffFn1 isWeekendImpl date

foreign import isWithinRangeImpl
  :: ∀ e
   . EffFn3 (DateFnsEff e) DateF DateF DateF Boolean

isWithinRange
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> DateF -> m Boolean
isWithinRange date dateStart dateEnd =
  liftEff $ runEffFn3 isWithinRangeImpl date dateStart dateEnd

foreign import isYesterdayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF Boolean

isYesterday
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m Boolean
isYesterday date =
  liftEff $ runEffFn1 isYesterdayImpl date


foreign import lastDayOfISOWeekImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

lastDayOfISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
lastDayOfISOWeek date =
  liftEff $ runEffFn1 lastDayOfISOWeekImpl date

foreign import lastDayOfISOYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

lastDayOfISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
lastDayOfISOYear date =
  liftEff $ runEffFn1 lastDayOfISOYearImpl date

foreign import lastDayOfMonthImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

lastDayOfMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
lastDayOfMonth date =
  liftEff $ runEffFn1 lastDayOfMonthImpl date

foreign import lastDayOfQuarterImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

lastDayOfQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
lastDayOfQuarter date =
  liftEff $ runEffFn1 lastDayOfQuarterImpl date

foreign import lastDayOfWeekImpl
  :: ∀ e opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) DateF

lastDayOfWeek
  :: ∀ e m opts opts_
   . Union opts opts_ LastDayOfWeekOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m DateF
lastDayOfWeek date options =
  liftEff $ runEffFn2 lastDayOfWeekImpl date options

foreign import lastDayOfYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

lastDayOfYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
lastDayOfYear date =
  liftEff $ runEffFn1 lastDayOfYearImpl date

foreign import maxImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) (Array DateF) DateF

max
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => (Array DateF) -> m DateF
max dates =
  liftEff $ runEffFn1 maxImpl dates

foreign import minImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) (Array DateF) DateF

min
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => (Array DateF) -> m DateF
min dates =
  liftEff $ runEffFn1 minImpl dates

foreign import parseImpl
  :: ∀ e opts
    . EffFn2 (DateFnsEff e) DateF (Record opts) DateF

parse
  :: ∀ e m opts opts_
   . Union opts opts_ ParseOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m DateF
parse argument options =
  liftEff $ runEffFn2 parseImpl argument options

foreign import setDateImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setDate
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setDate date dayOfMonth =
  liftEff $ runEffFn2 setDateImpl date dayOfMonth

foreign import setDayImpl
  :: ∀ e opts
   . EffFn3 (DateFnsEff e) DateF Int (Record opts) DateF

setDay
  :: ∀ e m opts opts_
   . Union opts opts_ SetDayOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Int -> Record opts -> m DateF
setDay date day options =
  liftEff $ runEffFn3 setDayImpl date day options

foreign import setDayOfYearImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setDayOfYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setDayOfYear date dayOfYear =
  liftEff $ runEffFn2 setDayOfYearImpl date dayOfYear

foreign import setHoursImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setHours date hours =
  liftEff $ runEffFn2 setHoursImpl date hours

foreign import setISODayImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setISODay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setISODay date day =
  liftEff $ runEffFn2 setISODayImpl date day

foreign import setISOWeekImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setISOWeek date week =
  liftEff $ runEffFn2 setISOWeekImpl date week

foreign import setISOYearImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setISOYear date year =
  liftEff $ runEffFn2 setISOYearImpl date year

foreign import setMillisecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setMilliseconds date milliseconds =
  liftEff $ runEffFn2 setMillisecondsImpl date milliseconds

foreign import setMinutesImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setMinutes date minutes =
  liftEff $ runEffFn2 setMinutesImpl date minutes

foreign import setMonthImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setMonth date month =
  liftEff $ runEffFn2 setMonthImpl date month

foreign import setQuarterImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setQuarter date quarter =
  liftEff $ runEffFn2 setQuarterImpl date quarter

foreign import setSecondsImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setSeconds date seconds =
  liftEff $ runEffFn2 setSecondsImpl date seconds

foreign import setYearImpl
  :: ∀ e
   . EffFn2 (DateFnsEff e) DateF Int DateF

setYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
setYear date year =
  liftEff $ runEffFn2 setYearImpl date year

foreign import startOfDayImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfDay
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfDay date =
  liftEff $ runEffFn1 startOfDayImpl date

foreign import startOfHourImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfHour
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfHour date =
  liftEff $ runEffFn1 startOfHourImpl date

foreign import startOfISOWeekImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfISOWeek
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfISOWeek date =
  liftEff $ runEffFn1 startOfISOWeekImpl date

foreign import startOfISOYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfISOYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfISOYear date =
  liftEff $ runEffFn1 startOfISOYearImpl date

foreign import startOfMinuteImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfMinute
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfMinute date =
  liftEff $ runEffFn1 startOfMinuteImpl date

foreign import startOfMonthImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfMonth
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfMonth date =
  liftEff $ runEffFn1 startOfMonthImpl date

foreign import startOfQuarterImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfQuarter
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfQuarter date =
  liftEff $ runEffFn1 startOfQuarterImpl date

foreign import startOfSecondImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfSecond
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfSecond date =
  liftEff $ runEffFn1 startOfSecondImpl date

foreign import startOfTodayImpl
  :: forall e
   . Eff (DateFnsEff e) DateF

startOfToday
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => m DateF
startOfToday = liftEff startOfTodayImpl

foreign import startOfTomorrowImpl
  :: forall e
   . Eff (DateFnsEff e) DateF

startOfTomorrow
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => m DateF
startOfTomorrow = liftEff startOfTomorrowImpl

foreign import startOfWeekImpl
  :: ∀ e opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) DateF

startOfWeek
  :: ∀ e m opts opts_
   . Union opts opts_ StartOfWeekOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> (Record opts) -> m DateF
startOfWeek date options =
  liftEff $ runEffFn2 startOfWeekImpl date options

foreign import startOfYearImpl
  :: ∀ e
   . EffFn1 (DateFnsEff e) DateF DateF

startOfYear
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> m DateF
startOfYear date =
  liftEff $ runEffFn1 startOfYearImpl date

foreign import startOfYesterdayImpl
  :: forall e
   . Eff (DateFnsEff e) DateF

startOfYesterday
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => m DateF
startOfYesterday = liftEff startOfYesterdayImpl


foreign import subDaysImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subDays
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subDays date amount =
  liftEff $ runEffFn2 subDaysImpl date amount

foreign import subHoursImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subHours
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subHours date amount =
  liftEff $ runEffFn2 subHoursImpl date amount

foreign import subISOYearsImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subISOYears
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subISOYears date amount =
  liftEff $ runEffFn2 subISOYearsImpl date amount

foreign import subMillisecondsImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subMilliseconds
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subMilliseconds date amount =
  liftEff $ runEffFn2 subMillisecondsImpl date amount

foreign import subMinutesImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subMinutes
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subMinutes date amount =
  liftEff $ runEffFn2 subMinutesImpl date amount

foreign import subMonthsImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subMonths
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subMonths date amount =
  liftEff $ runEffFn2 subMonthsImpl date amount

foreign import subQuartersImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subQuarters
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subQuarters date amount =
  liftEff $ runEffFn2 subQuartersImpl date amount

foreign import subSecondsImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subSeconds
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subSeconds date amount =
  liftEff $ runEffFn2 subSecondsImpl date amount

foreign import subWeeksImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subWeeks
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subWeeks date amount =
  liftEff $ runEffFn2 subWeeksImpl date amount

foreign import subYearsImpl
  :: forall e
   . EffFn2 (DateFnsEff e) DateF Int DateF

subYears
  :: forall e m
   . MonadEff (DateFnsEff e) m
  => DateF -> Int -> m DateF
subYears date amount =
  liftEff $ runEffFn2 subYearsImpl date amount
