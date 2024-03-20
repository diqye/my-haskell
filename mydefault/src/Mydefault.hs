module Mydefault where
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Control.Lens hiding ((.=))

default (Integer,String,Double)

(<<>>) :: A.Value -> A.Value -> A.Value
a <<>> b = a & _Object %~ (b ^. _Object <>)