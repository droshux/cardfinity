{-# LANGUAGE TemplateHaskell #-}

module Context (Context, theme, initialCtx) where

import Miso.Lens.TH (makeLenses)
import Theme.Types qualified as Theme

newtype Context = Context
  { _theme :: Theme.Theme
  }
  deriving (Eq)

$(makeLenses ''Context)

initialCtx :: Context
initialCtx = Context {_theme = Theme.defaultTheme}
