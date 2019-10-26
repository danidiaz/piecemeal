{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Piecemeal
  ( Complete (..),
    Incomplete (..),
    Piece (..),
    PiecemealAPI,
    Patcheable,
    piecemealApp,
    defaultMain,
  )
where

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
import Data.SOP.NP (collapse_NP, cpure_NP, liftA2_NP, pure_NP, sequence_NP)
import Data.SOP.NS (collapse_NS, expand_NS, liftA_NS)
import Data.String
import GHC.TypeLits
import Network.Wai
import Network.Wai.Handler.Warp
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

newtype Mendo a = Mendo {getMendo :: Maybe a -> Maybe a}

type Patcheable (t :: Map Symbol Type) flat =
  ( KeysValuesAll KnownKey t,
    Productlike '[] t flat,
    Sumlike '[] t flat,
    All FromJSON flat,
    All ToJSON flat
  )

-- https://hackage.haskell.org/package/servant-0.4.2/docs/Servant-API-Patch.html
piecemealApp ::
  forall (t :: Map Symbol Type) flat.
  Patcheable t flat =>
  MVar (Incomplete t) ->
  Application
piecemealApp ref = serve (Proxy @(PiecemealAPI t)) (consult :<|> patch)
  where
    consult = liftIO $ do
      i@(Incomplete current) <- readMVar ref
      return
        ( case sequence_NP (toNP current) of
            Just filled -> Left (Complete (fromNP filled))
            Nothing -> Right i
        )
    patch (Piece (toNS -> piece)) = liftIO $ do
      -- https://stackoverflow.com/questions/58573934/updating-an-n-ary-product-from-sop-core-with-a-compatible-sum
      let mendos :: NP Mendo flat
          mendos = expand_NS (Mendo id) (liftA_NS (\(I x) -> Mendo (\_ -> Just x)) piece)
          adjust :: NP Maybe flat -> NP Maybe flat
          adjust = liftA2_NP (\(Mendo f) x -> f x) mendos
      modifyMVar_ ref $ return . Incomplete . fromNP . adjust . toNP . getIncomplete

defaultMain :: forall t flat. Patcheable t flat => Proxy (t :: Map Symbol Type) -> IO ()
defaultMain _ = do
  ref <- newMVar (Incomplete (fromNP @t (pure_NP Nothing)))
  let port = 8081
  putStrLn $ "Listening on port " ++ show port
  run port (piecemealApp ref)
