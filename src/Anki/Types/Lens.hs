{-# LANGUAGE TemplateHaskell #-}

module Anki.Types.Lens where

import Anki.Types
import Lens.Micro.TH

makeFields ''AddNoteParams
makeFields ''Media