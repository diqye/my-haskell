{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Myai.Data.Azure where

import Control.Lens ( makeLenses )
import Data.Default.Class ( Default )
import GHC.Generics ( Generic )

data Azure = Azure {
    _key :: String,
    _endpoint :: String
} deriving (Show, Generic)

makeLenses ''Azure

instance Default Azure