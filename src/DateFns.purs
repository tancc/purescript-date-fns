module DateFns where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn2, mkEffFn2, runEffFn2)
import Unsafe.Coerce (unsafeCoerce)

-- foreign import data DateF :: Type

instance showDate :: Show DateF where
  show = unsafeCoerce

type Year      = Int
type Month     = Int
type Date      = Int
type Timestamp = Int
type ISO8016   = String

data DateF = DateF | Timestamp | ISO8016

foreign import data DATEFNS ∷ Effect

foreign import now :: ∀ e. Eff (datefns ∷ DATEFNS | e) DateF

foreign import createDate :: ∀ e. Year -> Month -> Date -> Eff (datefns ∷ DATEFNS | e) DateF

foreign import addDaysImpl
  :: ∀ a m e
   . EffFn2 (datefns ∷ DATEFNS, exception ∷ EXCEPTION | e) DateF Int DateF

addDays
  :: ∀ m e
   . MonadEff (datefns ∷ DATEFNS, exception ∷ EXCEPTION | e) m
  => DateF
  -> Int
  -> m DateF
addDays date amount = liftEff $ runEffFn2 addDaysImpl date amount

main = do
  d <- now
  addDays d 10
