{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
  ( Pair,
    Parser,
    explicitParseField,
  )
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.Profunctor (Star (..))
import Data.RBR
  ( KeysValuesAll,
    KnownKey,
    Map,
    Productlike,
    Record,
    Sumlike,
    Variant,
    demoteKeys,
    fromNP,
    fromNS,
    toNP,
    toNS,
  )
import Data.SOP
  ( All,
    I (..),
    K (..),
    NP (..),
    NS (..),
    apFn,
    hcliftA2,
    unI,
    unK,
  )
import Data.SOP.NP
  ( collapse_NP,
    cpure_NP,
    liftA2_NP,
    pure_NP,
    sequence_NP,
  )
import Data.SOP.NS
  ( collapse_NS,
    expand_NS,
    injections,
    liftA_NS,
  )
import Data.String
import GHC.TypeLits (Symbol)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API

-- Losts of boring constraints grouped here for simplicity.
-- "flat" is the flattened representation of the types in the Map.
type Patcheable (t :: Map Symbol Type) (flat :: [Type]) =
  ( KeysValuesAll KnownKey t, -- we can demote the keys
    Productlike '[] t flat,
    Sumlike '[] t flat,
    All FromJSON flat,
    All ToJSON flat
  )

newtype Complete (t :: Map Symbol Type) = Complete {getComplete :: Record I t}

instance
  Patcheable t flat =>
  ToJSON (Complete t)
  where
  toJSON (Complete r) =
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName :: forall x kv . _ => K String x -> I x -> K kv x
        giveFieldName (K alias) (I fieldValue) = K (fromString alias .= fieldValue)
        pairs :: NP (K Pair) flat
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (collapse_NP pairs)

newtype Incomplete (t :: Map Symbol Type) = Incomplete {getIncomplete :: Record Maybe t}

instance
  Patcheable t flat =>
  ToJSON (Incomplete t)
  where
  toJSON (Incomplete r) =
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        giveFieldName :: forall x kv . _ => K String x -> Maybe x -> K kv x
        giveFieldName (K alias) maybeMissingFieldValue = K (fromString alias .= maybeMissingFieldValue)
        pairs :: NP (K Pair) flat
        pairs = hcliftA2 (Proxy @ToJSON) giveFieldName fieldNames (toNP r)
     in object (collapse_NP pairs)

newtype Piece (t :: Map Symbol Type) = Piece {getPiece :: Variant I t}

instance
  Patcheable t flat =>
  FromJSON (Piece t)
  where
  parseJSON =
    let fieldNames :: NP (K String) flat
        fieldNames = toNP (demoteKeys @t)
        fieldParsers = cpure_NP (Proxy @FromJSON) (Star parseJSON)
        giveFieldName :: K String b -> Star Parser Value c -> Star Parser Object c
        giveFieldName (K alias) (Star f) = Star (\o -> Data.Aeson.Types.explicitParseField f o (fromString alias))
        branchParsers :: NP (Star Parser Object) flat
        branchParsers = liftA2_NP giveFieldName fieldNames fieldParsers
        injected :: NP (K (Star Parser Object (NS I flat))) flat
        injected = liftA2_NP (\f star -> K (unK . apFn f . I <$> star)) (injections @flat) branchParsers
        parser :: Object -> Parser (NS I flat)
        Star parser = asum $ collapse_NP injected
     in withObject "piece" $ \o -> Piece . fromNS <$> parser o

-- https://docs.servant.dev/en/stable/tutorial/ApiType.html
type PiecemealAPI (t :: Map Symbol Type) =
  "piecemeal"
    :> ( Get '[JSON] (Either (Complete t) (Incomplete t))
           :<|> ReqBody '[JSON] (Piece t) :> Patch '[JSON] ()
       )

newtype Mendo a = Mendo {getMendo :: Maybe a -> Maybe a}

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

defaultMain :: forall (t :: Map Symbol Type) flat. Patcheable t flat => Proxy t -> IO ()
defaultMain _ = do
  ref <- newMVar (Incomplete (fromNP @t (pure_NP Nothing)))
  let port = 8081
  putStrLn $ "Listening on port " ++ show port
  run port (piecemealApp ref)
