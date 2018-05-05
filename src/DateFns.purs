module DateFns where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, EffFn4, runEffFn2, runEffFn3, runEffFn4)
import DateFns.Types (DateF, DateFnsEff, Day, DistanceInWordsOpts, DistanceInWordsStrictOpts, Month, Options, Year, DistanceInWordsToNowOpts)
import Prelude (($))


-- base methods
foreign import now :: ∀ e. Eff (DateFnsEff e) DateF
-- @TODO 能加入更多參數 小時-分-秒 等等
foreign import createDate :: ∀ e. Year -> Month -> Day -> Eff (DateFnsEff e) DateF

foreign import unsafeCreateDate :: ∀ a e. a -> Eff (DateFnsEff e) DateF

-- date-fns methods
foreign import addDaysImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addDays date amount = liftEff $ runEffFn2 addDaysImpl date amount

foreign import addHoursImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addHours date amount = liftEff $ runEffFn2 addHoursImpl date amount

foreign import addISOYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addISOYears date amount = liftEff $ runEffFn2 addISOYearsImpl date amount

foreign import addMillisecondsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMilliseconds date amount = liftEff $ runEffFn2 addMillisecondsImpl date amount

foreign import addMinutesImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMinutes date amount = liftEff $ runEffFn2 addMinutesImpl date amount

foreign import addMonthsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addMonths date amount = liftEff $ runEffFn2 addMonthsImpl date amount

foreign import addQuartersImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addQuarters date amount = liftEff $ runEffFn2 addQuartersImpl date amount

foreign import addSecondsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addSeconds date amount = liftEff $ runEffFn2 addSecondsImpl date amount

foreign import addWeeksImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addWeeks date amount = liftEff $ runEffFn2 addWeeksImpl date amount

foreign import addYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF Int DateF

addYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF
  -> Int
  -> m DateF
addYears date amount = liftEff $ runEffFn2 addYearsImpl date amount

foreign import areRangesOverlappingImpl
  :: ∀ a e m
   . EffFn4 (DateFnsEff e) DateF DateF DateF DateF Boolean

areRangesOverlapping
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> DateF -> DateF -> m Boolean
areRangesOverlapping initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate =
  liftEff $ runEffFn4 areRangesOverlappingImpl initialRangeStartDate initialRangeEndDate comparedRangeStartDate comparedRangeEndDate

foreign import closestIndexToImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF (Array DateF) Int

closestIndexTo
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> (Array DateF) -> m Int
closestIndexTo dateToCompare datesArray =
  liftEff $ runEffFn2 closestIndexToImpl dateToCompare datesArray

foreign import closestToImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF (Array DateF) DateF

closestTo
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> (Array DateF) -> m DateF
closestTo dateToCompare datesArray =
  liftEff $ runEffFn2 closestToImpl dateToCompare datesArray

foreign import compareAscImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF DateF

compareAsc
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m DateF
compareAsc dateLeft dateRight =
  liftEff $ runEffFn2 compareAscImpl dateLeft dateRight

foreign import compareDescImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF DateF

compareDesc
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m DateF
compareDesc dateLeft dateRight =
  liftEff $ runEffFn2 compareDescImpl dateLeft dateRight

foreign import differenceInCalendarDaysImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarDays dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarDaysImpl dateLeft dateRight

foreign import differenceInCalendarISOWeeksImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarISOWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarISOWeeks dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarISOWeeksImpl dateLeft dateRight

foreign import differenceInCalendarISOYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarISOYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarISOYearsImpl dateLeft dateRight

foreign import differenceInCalendarMonthsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarMonths dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarMonthsImpl dateLeft dateRight

foreign import differenceInCalendarQuartersImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarQuarters dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarQuartersImpl dateLeft dateRight

foreign import differenceInCalendarWeeksImpl
  :: ∀ a e m opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) Int

differenceInCalendarWeeks
  :: ∀ e m opts opts_
   . Union opts opts_ Options
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m Int
differenceInCalendarWeeks dateLeft dateRight options =
  liftEff $ runEffFn3 differenceInCalendarWeeksImpl dateLeft dateRight options

foreign import differenceInCalendarYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInCalendarYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInCalendarYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInCalendarYearsImpl dateLeft dateRight

foreign import differenceInDaysImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInDays
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInDays dateLeft dateRight =
  liftEff $ runEffFn2 differenceInDaysImpl dateLeft dateRight

foreign import differenceInHoursImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInHours
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInHours dateLeft dateRight =
  liftEff $ runEffFn2 differenceInHoursImpl dateLeft dateRight

foreign import differenceInISOYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInISOYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInISOYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInISOYearsImpl dateLeft dateRight

foreign import differenceInMillisecondsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMilliseconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMilliseconds dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMillisecondsImpl dateLeft dateRight

foreign import differenceInMinutesImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMinutes
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMinutes dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMinutesImpl dateLeft dateRight

foreign import differenceInMonthsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInMonths
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInMonths dateLeft dateRight =
  liftEff $ runEffFn2 differenceInMonthsImpl dateLeft dateRight

foreign import differenceInQuartersImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInQuarters
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInQuarters dateLeft dateRight =
  liftEff $ runEffFn2 differenceInQuartersImpl dateLeft dateRight

foreign import differenceInSecondsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInSeconds
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInSeconds dateLeft dateRight =
  liftEff $ runEffFn2 differenceInSecondsImpl dateLeft dateRight

foreign import differenceInWeeksImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInWeeks
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInWeeks dateLeft dateRight =
  liftEff $ runEffFn2 differenceInWeeksImpl dateLeft dateRight

foreign import differenceInYearsImpl
  :: ∀ a e m
   . EffFn2 (DateFnsEff e) DateF DateF Int

differenceInYears
  :: ∀ e m
   . MonadEff (DateFnsEff e) m
  => DateF -> DateF -> m Int
differenceInYears dateLeft dateRight =
  liftEff $ runEffFn2 differenceInYearsImpl dateLeft dateRight

foreign import distanceInWordsImpl
  :: ∀ a e m opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) String

distanceInWords
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m String
distanceInWords dateLeft dateRight options =
  liftEff $ runEffFn3 distanceInWordsImpl dateLeft dateRight options

foreign import distanceInWordsStrictImpl
  :: ∀ a e m opts
   . EffFn3 (DateFnsEff e) DateF DateF (Record opts) String

distanceInWordsStrict
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsStrictOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> DateF -> Record opts -> m String
distanceInWordsStrict dateLeft dateRight options =
  liftEff $ runEffFn3 distanceInWordsStrictImpl dateLeft dateRight options

foreign import distanceInWordsToNowImpl
  :: ∀ a e m opts
   . EffFn2 (DateFnsEff e) DateF (Record opts) String

distanceInWordsToNow
  :: ∀ e m opts opts_
   . Union opts opts_ DistanceInWordsToNowOpts
  => MonadEff (DateFnsEff e) m
  => DateF -> Record opts -> m String
distanceInWordsToNow date options =
  liftEff $ runEffFn2 distanceInWordsToNowImpl date options
