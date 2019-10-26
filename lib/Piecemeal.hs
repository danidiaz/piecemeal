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

import Data.Aeson
import Data.RBR
import Data.SOP
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

instance (Sumlike '[] t flat, All FromJSON flat) =>
  FromJSON (Piece t)
  where
  parseJSON v = undefined

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
