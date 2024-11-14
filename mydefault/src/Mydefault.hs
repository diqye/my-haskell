{-# LANGUAGE InstanceSigs #-}
module Mydefault where
import qualified Data.Aeson as A
import Data.Aeson.Lens ( AsValue(_Object) )
import Control.Lens ( (&), (^.), (%~) )

default (Int,String,Float)


instance Semigroup A.Value where
    -- | v1 <> v2 v1 为主 连接/覆盖 v2
    (<>) :: A.Value -> A.Value -> A.Value
    (A.Object v1) <> (A.Object v2) = A.Object (v1 <> v2)
    (A.Array a1) <> (A.Array a2) = A.Array (a1 <> a2)
    (A.String a1) <> (A.String a2) = A.String (a1 <> a2)
    v1 <> _ = v1


(=:) :: (A.KeyValue kv,A.ToJSON v) => A.Key -> v -> kv
(=:) = (A..=)