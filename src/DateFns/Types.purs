module DateFns.Types where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Locale :: Type

type Year      = Int
type Month     = Int
type Day       = Int
type Timestamp = Int
type ISO8016   = String
type Date      = String

data DateF = Date | Timestamp | ISO8016

instance showDate :: Show DateF where
  show :: DateF -> String
  show = unsafeCoerce

foreign import data DATEFNS :: Effect

type DateFnsEff e = (datefns :: DATEFNS, exception :: EXCEPTION | e)

-- https://date-fns.org/docs/Options
type Options = (
                 weekStartsOn     :: Int
               , additionalDigits :: Int
               , includeSeconds   :: Boolean
               , addSuffix        :: Boolean
               , unit             :: String
               , roundingMethod   :: String
               , locale           :: Locale
               )

type DistanceInWordsOpts = ( includeSeconds :: Boolean
                           , addSuffix      :: Boolean
                           , locale         :: Locale
                           )

type DistanceInWordsToNowOpts = DistanceInWordsOpts

type DistanceInWordsStrictOpts = ( addSuffix     :: Boolean
                                 , unit          :: String
                                 , partialMethod :: String
                                 , locale        :: Locale
                                 )

type EndOfWeekOpts = (weekStartsOn :: Int)

type FormatOpts = (locale :: Locale)

type IsSameWeekOpts = (weekStartsOn :: Int)

type IsThisWeekOpts = (weekStartsOn :: Int)

type LastDayOfWeekOpts = (weekStartsOn :: Int)

type ParseOpts = (additionalDigits :: Int)

type SetDayOpts = (weekStartsOn :: Int)

type StartOfWeekOpts = (weekStartsOn :: Int)