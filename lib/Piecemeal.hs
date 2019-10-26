{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Piecemeal where

import Servant.API
import Data.RBR
import Data.SOP

newtype Incomplete r = Incomplete { getIncomplete :: Record Maybe (RecordCode r) }

newtype Piece r = Piece { getPiece :: Variant I (RecordCode r) }

-- https://docs.servant.dev/en/stable/tutorial/ApiType.html
type PiecemealAPI r =
  "piecemeal"
    :> ( Get '[JSON] (Either r (Incomplete r))
           :<|> Patch '[JSON] (Piece r)
       )
