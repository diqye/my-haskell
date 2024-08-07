{-# LANGUAGE TemplateHaskell #-}
module Myai.Data.Config where

import qualified Myai.Data.GPT as G
import qualified Myai.Data.Azure as Az
import Control.Monad.Reader ( MonadReader )

import Data.Monoid ( First )
import Data.Aeson ( Value ) 
import HTTP.Myrequest ( newTlsManager, Manager )
import Data.Default.Class ( Default(..) )
import Control.Monad.Except ( MonadError )
import Text.Printf ( printf )
import Control.Lens ( makeLenses )
import Control.Applicative((<|>),Alternative)



data Config = Config {
    _manager :: IO Manager ,
    _gpt :: G.GPT,
    _azure :: Az.Azure
}

instance Show Config where
    show (Config {_gpt=gpt,_azure=azure}) = printf 
        "Config { _manager=<IO manager>, _gpt= %s, _azure= %s}"
        (show gpt)
        (show azure)


instance Default Config where
    def = Config {
        _manager = newTlsManager ,
        _gpt = def,
        _azure = def
    }

makeLenses ''Config

type Error = First Value
type MonadAIReader m = MonadReader Config m
type MonadAIError m = MonadError Error m
type MonadAI m = (Alternative m, MonadAIReader m, MonadAIError m)