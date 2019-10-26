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

module Piecemeal where

import Data.Aeson
import Data.RBR
import Data.SOP
import Servant
import Servant.API

newtype Incomplete r = Incomplete {getIncomplete :: Record Maybe (RecordCode r)}

newtype Piece r = Piece {getPiece :: Variant I (RecordCode r)}

-- https://docs.servant.dev/en/stable/tutorial/ApiType.html
type PiecemealAPI r =
  "piecemeal"
    :> ( Get '[JSON] (Either r (Incomplete r))
           :<|> ReqBody '[JSON] (Piece r) :> Patch '[JSON] ()
       )

-- https://hackage.haskell.org/package/servant-0.4.2/docs/Servant-API-Patch.html
--  _foo1 :: Handler (Either r (Incomplete r))
--  _foo2 :: Piece r -> Handler ()
piecemealApp ::
  forall r.
  (ToJSON r, ToJSON (Incomplete r), FromJSON (Piece r)) =>
  Proxy r ->
  Application
piecemealApp _ = serve (Proxy @(PiecemealAPI r)) (_foo1 :<|> _foo2)
