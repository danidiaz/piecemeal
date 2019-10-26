{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Piecemeal where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Kind
import Data.Profunctor (Star (..))
import Data.RBR
import Data.SOP
import Data.SOP.NP (collapse_NP, cpure_NP, liftA2_NP, sequence_NP)
import Data.String
import GHC.TypeLits
import Servant
import Servant.API

newtype Complete (t :: Map Symbol Type) = Complete {getComplete :: Record I t}

instance
  (KeysValuesAll KnownKey t, Productlike '[] t flat, All ToJSON flat) =>
  ToJSON (Complete t)
  where
  toJSON (Complete r) =
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName (K alias) (I fieldValue) = K (fromString alias .= fieldValue)
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (hcollapse pairs)

newtype Incomplete (t :: Map Symbol Type) = Incomplete {getIncomplete :: Record Maybe t}

instance
  (KeysValuesAll KnownKey t, Productlike '[] t flat, All ToJSON flat) =>
  ToJSON (Incomplete t)
  where
  toJSON (Incomplete r) =
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName (K alias) maybeMissingFieldValue = K (fromString alias .= maybeMissingFieldValue)
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (hcollapse pairs)

newtype Piece (t :: Map Symbol Type) = Piece {getPiece :: Variant I t}

instance
  (KeysValuesAll KnownKey t, Productlike '[] t flat, Sumlike '[] t flat, All FromJSON flat) =>
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
--  http://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server.html#t:Handler
--  runHandler' :: ExceptT ServerError IO a
piecemealApp ::
  forall (t :: Map Symbol Type) flat.
  (KeysValuesAll KnownKey t, Productlike '[] t flat, Sumlike '[] t flat, All FromJSON flat, All ToJSON flat) =>
  MVar (Incomplete t) ->
  Application
piecemealApp ref = serve (Proxy @(PiecemealAPI t)) (consult :<|> patch)
  where
    consult = liftIO $ do
      i@(Incomplete current) <- readMVar ref
      return (case sequence_NP (toNP current) of
        Just filled -> Left (Complete (fromNP filled))
        Nothing -> Right i)
    patch i = liftIO $ do
      undefined
