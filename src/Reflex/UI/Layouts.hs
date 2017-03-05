{-# LANGUAGE OverloadedLists #-}
module Reflex.UI.Layouts where

import Reflex
import qualified Reflex.Dom as Dom

import Reflex.UI.Elements

columns :: Dom.DomBuilder t m => Attrs -> m a -> m a
columns attrs = div' (addClass "columns" attrs)

column :: Dom.DomBuilder t m => Attrs -> m a -> m a
column attrs = div' (addClass "column" attrs)

col :: Dom.DomBuilder t m => m a -> m a
col = div' ["class" =: "column"]
