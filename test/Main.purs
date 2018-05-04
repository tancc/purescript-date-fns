module Test.Main where

import DateFns

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Traversable (sequence)
import DateFns.Types (DateFnsEff)
import Prelude (class Show, Unit, bind, discard, show, ($), (<>), (>>=))

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
    initialRangeStartDate  <- createDate 2014 0 10
    initialRangeEndDate    <- createDate 2014 0 20
    comparedRangeStartDate <- createDate 2014 0 17
    comparedRangeEndDate   <- createDate 2014 0 21
    areRangesOverlapping initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate >>= (myLog "areRangesOverlapping")
  do
    dateToCompare <- createDate 2015 8 6
    datesArray    <- sequence [createDate 2015 0 1, createDate 2016 0 1, createDate 2017 0 1]
    closestIndexTo dateToCompare datesArray >>= (myLog "closestIndexTo")
  do
    dateToCompare <- createDate 2015 8 6
    datesArray    <- sequence [createDate 2000 0 1, createDate 2030 0 1]
    closestTo dateToCompare datesArray >>= (myLog "closestTo")
  do
    dateLeft  <- createDate 1987 1 11
    dateRight <- createDate 1989 6 10
    compareAsc dateLeft dateRight >>= (myLog "compareAsc")
  do
    dateLeft  <- createDate 1987 1 11
    dateRight <- createDate 1989 6 10
    compareDesc dateLeft dateRight >>= (myLog "compareDesc")

