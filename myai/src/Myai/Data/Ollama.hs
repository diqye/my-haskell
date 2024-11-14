{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Myai.Data.Ollama where

import Control.Lens ( makeLenses )
import Data.Default.Class ( Default )
import GHC.Generics ( Generic )

newtype Ollama = Ollama {
    _baseURL :: String
} deriving (Show, Generic)

makeLenses ''Ollama

instance Default Ollama