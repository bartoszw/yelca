{-

A common practice in many Haskell applications is to define a
helper module for each project that provides commonly needed
imports. The purpose of this module is purely convenience.

-}
module Import (
    module Import,
--    module Import1,
--    module Yesod,
--    module ClassyPrelude.Yesod,
--    module Yesod.Default.Util,
 --   module Yesod.Form.Jquery,
--    module Yesod.Static,
--    module Foundation,
--    module Data.Text,
    module Data.Time,
    module Control.Applicative,
    module Control.Arrow,
    module Control.Monad,
    module Data.Maybe,
--    module Data.Monoid,
    module Data.Char,
    module Data.Function,
--    module Data.List,
    module Text.ReadEither,
    module Text.PrettyShow,
    module Text.Julius,
    module Text.Read
    )
    where

import           Import.NoFoundation as Import
import           Data.Default
import           Foundation          as Import
import           Language.Haskell.TH
--import           Yesod               --as X
--import           Yesod.Default.Util
--import           ClassyPrelude.Yesod
--import           Yesod.Core.Content
--import           Yesod.Static
--import           Yesod.Form.Jquery  -- as X (urlJqueryJs)

--widgetFile :: FilePath -> ExpQ
--widgetFile = widgetFileReload def

import           Control.Applicative ((<$>), (<*>), (<$))
import           Control.Arrow ((&&&))
import           Control.Monad (liftM,return)
--import           Data.Text           (Text,pack,unpack)
--import qualified Data.Text  as T
import           Data.Time           (Day)
import           Data.Char (isAlpha)
import           Data.Maybe (isJust, fromMaybe, fromJust)
import           Data.Function (($))
--import           Data.Monoid ((<>))
--import           Data.List (foldl')
import Text.Hamlet
import Text.Julius
import Text.ReadEither
import Text.PrettyShow
import Text.Read (Read,read)

--widgetFile :: FilePath -> ExpQ
--widgetFile = widgetFileReload def
