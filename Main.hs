{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Piecemeal

import Data.Proxy
import Data.RBR (ToRecord,RecordCode)
import GHC.Generics

data Person = Person { name :: String, age :: Int } 
    deriving (Show,Generic,ToRecord)

main :: IO ()
main = defaultMain (Proxy @(RecordCode Person))
