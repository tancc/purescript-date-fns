module DateFns where

import Prelude (($))

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4)
import DateFns.Types (DateF, DateFnsEff, Day, Month, Year)


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
