module DateFns.Types where

import Prelude

import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Unsafe.Coerce (unsafeCoerce)

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
              --  @TODO
              --  , local
               )