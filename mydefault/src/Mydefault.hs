{-# LANGUAGE InstanceSigs #-}
module Mydefault where
import qualified Data.Aeson as A
import Data.Aeson.Lens ( AsValue(_Object) )
import Control.Lens ( (&), (^.), (%~) )

default (Int,String,Float)

-- | a <<>> b  b 覆盖 a
(<<>>) :: A.Value -> A.Value -> A.Value
a <<>> b = a & _Object %~ (b ^. _Object <>)

instance Semigroup A.Value where
    -- | v1 <> v2 v1 覆盖 v2
    (<>) :: A.Value -> A.Value -> A.Value
    (A.Object v1) <> (A.Object v2) = A.Object (v1 <> v2)
    v1 <> _ = v1


(=:) :: (A.KeyValue kv,A.ToJSON v) => A.Key -> v -> kv
(=:) = (A..=)