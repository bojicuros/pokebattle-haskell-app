{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types
  ( Pokemon (..),
    Candy (..),
    Battle(..),
    Pokemonrec (..)
  )
where

import Control.Exception (IOException)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Pokemon = Pokemon
  { id :: Int,
    name :: String,
    img :: String,
    type_ :: String,
    height :: String,
    weight :: String,
    candy_id :: Int,
    weakness :: String
  }

instance Show Pokemon where
  show pokemon =
    "Id: " ++ show (id pokemon)
      ++ " Name: "
      ++ name pokemon
      ++ " Type: "
      ++ type_ pokemon
      ++ " Height: "
      ++ height pokemon
      ++ " Weight: "
      ++ weight pokemon
      ++ " Weakness: "
      ++ weakness pokemon

data Candy = Candy
  { id_ :: Int,
    name_ :: String,
    quantity :: Int,
    img_ :: String
  }

instance Show Candy where
  show candy =
    mconcat
      [ show $ id_ candy,
        " ",
        name_ candy,
        " ",
        show $ quantity candy
      ]

data Pokemonrec = Pokemonrec
  { pokemon :: String
  }
  deriving (Show, Generic)

data Battle = Battle
  { iD:: Int,
    frst :: Int,
    secn :: Int,
    winn :: Int
  }