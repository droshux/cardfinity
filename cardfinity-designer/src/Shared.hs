{-# LANGUAGE OverloadedStrings #-}

module Shared where

import Editor.Types
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Types qualified as CF

triggerIcon :: (IsTrigger a) => a -> M.View parent action
triggerIcon t =
  H.img_
    [ CSS.style_ [CSS.width $ CSS.em 1.0],
      P.src_ $ iconSrc $ fromTrigger $ toTrigger t
    ]
  where
    iconSrc :: TriggerID -> M.MisoString
    iconSrc t = "assets/icons/trigger/" <> M.toMisoString t <> ".svg"
