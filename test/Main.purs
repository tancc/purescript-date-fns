module Test.Main where

import DateFns

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Traversable (sequence)
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
  addDays d 10 >>= (myLog "addDays")
  addHours d 10 >>= (myLog "addHours")
  addISOYears d 5 >>= (myLog "addISOYears")
  addMilliseconds d 750 >>= (myLog "addMilliseconds")
  addMinutes d 30 >>= (myLog "addMinutes")
  addMonths d 30 >>= (myLog "addMonths")
  addQuarters d 1 >>= (myLog "addQuarters")
  addSeconds d 30 >>= (myLog "addSeconds")
  addWeeks d 4 >>= (myLog "addWeeks")
  addYears d 2 >>= (myLog "addYears")
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