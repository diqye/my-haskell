{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Myai.Data.GPT where
import Data.Default.Class ( Default )
import Control.Lens ( makeLenses )
import GHC.Generics ( Generic )

data GPT = GPT {
    _key :: String,
    _baseUrl :: String
} deriving (Show,Generic)

makeLenses ''GPT

instance Default GPT