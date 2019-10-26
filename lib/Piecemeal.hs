{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Piecemeal where

import Data.Profunctor (Star(..))
import Data.Aeson
import Data.Aeson.Types
import Data.RBR
import Data.SOP 
import Data.SOP.NP (cpure_NP,collapse_NP,liftA2_NP)
import Control.Applicative
import Data.Foldable
import Servant
import Servant.API
import GHC.TypeLits
import Data.Kind
import Data.String

newtype Complete (t :: Map Symbol Type)  = Complete { getComplete :: Record I t }

instance (KeysValuesAll KnownKey t,Productlike '[] t flat, All ToJSON flat) =>
  ToJSON (Complete t)
  where
  toJSON (Complete r) = 
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName (K alias) (I fieldValue) = K (fromString alias .= fieldValue)
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (hcollapse pairs)

newtype Incomplete (t :: Map Symbol Type)  = Incomplete { getIncomplete :: Record Maybe t }

instance (KeysValuesAll KnownKey t,Productlike '[] t flat, All ToJSON flat) =>
  ToJSON (Incomplete t)
  where
  toJSON (Incomplete r) = 
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName (K alias) maybeMissingFieldValue = K (fromString alias .= maybeMissingFieldValue)
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (hcollapse pairs)

newtype Piece (t :: Map Symbol Type)  = Piece { getPiece :: Variant I t }

instance (KeysValuesAll KnownKey t,Productlike '[] t flat, Sumlike '[] t flat, All FromJSON flat) =>
    FromJSON (Piece t)
    where
    parseJSON = 
        let fieldNames :: NP (K String) flat
            fieldNames = toNP (demoteKeys @t)
            fieldParsers = cpure_NP (Proxy @FromJSON) (Star parseJSON)
            giveFieldName (K alias) (Star f) = Star (\o -> Data.Aeson.Types.explicitParseField f o (fromString alias))
            branchParsers :: NP (Star Parser Object) flat
            branchParsers = liftA2_NP giveFieldName fieldNames fieldParsers
            injected = liftA2_NP (\f star -> K (unK . apFn f . I <$> star)) (injections @flat) branchParsers 
            Star parser = asum $ collapse_NP injected
         in withObject "piece" $ \o -> Piece . fromNS <$> parser o

-- https://docs.servant.dev/en/stable/tutorial/ApiType.html
type PiecemealAPI (t :: Map Symbol Type) =
  "piecemeal"
    :> ( Get '[JSON] (Either (Complete t) (Incomplete t))
           :<|> ReqBody '[JSON] (Piece t) :> Patch '[JSON] ()
       )

-- https://hackage.haskell.org/package/servant-0.4.2/docs/Servant-API-Patch.html
-- _foo1 :: Handler (Either (Complete t) (Incomplete t))
--  _foo2 :: Piece t -> Handler ()
piecemealApp ::
  forall t.
  (ToJSON (Complete t), ToJSON (Incomplete t), FromJSON (Piece t)) =>
  Proxy t ->
  Application
piecemealApp _ = serve (Proxy @(PiecemealAPI t)) (_foo1 :<|> _foo2)
