{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PokeTypes (getWinner,
                haveType,
                getPokemonAtIndex,
                getPokemonFromString,
                getCandyFromString,
                getPokemonFromCandyId,
                evolutionOfPokemon,
                getInt,
                addStronger
) where

import Types as T(Pokemon (..),Candy (..),Battle(..))
import Database (addBattle)
import Data.Char(digitToInt)
import Prelude hiding (id)


data PokeType = Grass
                | Poison
                | Fire
                | Ice
                | Flying
                | Psychic
                | Water
                | Ground
                | Rock
                | Electric
                | Bug
                | Normal
                | Dragon
                | Fairy
                | Dark
                | Fighting
                | Ghost
                | Steel
                deriving Show

getType :: String -> PokeType
getType s | s=="Grass" = Grass
        | s=="Poison" = Poison
        | s=="Fire" = Fire
        | s=="Ice" = Ice
        | s=="Flying" = Flying
        | s=="Psychic" = Psychic
        | s=="Water" = Water
        | s=="Ground" = Ground
        | s=="Rock" = Rock
        | s=="Electric" = Electric
        | s=="Bug" = Bug
        | s=="Normal" = Normal
        | s=="Dragon" = Dragon
        | s=="Fairy" = Fairy
        | s=="Dark" = Dark
        | s=="Fighting" = Fighting
        | s=="Ghost" = Ghost
        | s=="Steel" = Steel

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
                where (w, s'') = break p s'

getTypes :: [String] -> [PokeType]
getTypes = map getType

typesOfPokemon :: Pokemon -> [PokeType]
typesOfPokemon pokemon = getTypes (wordsWhen (==',') (type_ pokemon))

weaknessOfPokemon :: Pokemon -> [PokeType]
weaknessOfPokemon pokemon = getTypes (wordsWhen (==',') (weakness pokemon))

containsType :: [PokeType] -> PokeType -> Bool
containsType xs t = foldr (\ x -> (||) (show t == show x)) False xs

haveType :: String -> Pokemon -> Bool
haveType tip pok = containsType (typesOfPokemon pok) (getType tip)

equalTypes :: [PokeType] -> [PokeType] -> Bool
equalTypes xs ys = foldr ((||) . containsType ys) False xs

isWeaker :: Pokemon -> Pokemon -> Bool
isWeaker p1 p2 = equalTypes (weaknessOfPokemon p1) (typesOfPokemon p2)

fight :: Pokemon -> Pokemon -> Pokemon
fight p1 p2 | isWeaker p1 p2 == isWeaker p2 p1 = if height p1 > height p2 then p1 else p2
            | isWeaker p2 p1 = p1
            | otherwise = p2
            
getWinner :: [Pokemon] -> String -> String -> Pokemon
getWinner pokemons p1 p2 = fight (getPokemonFromString pokemons p1) (getPokemonFromString pokemons p2)

addStronger :: [Pokemon] -> String -> String -> IO [Battle]
addStronger xs p1 p2 = addBattle (id (getPokemonFromString xs p1)) (id (getPokemonFromString xs p2)) (id (getWinner xs p1 p2))

getInt :: String -> Int -> Int
getInt "" i = i
getInt s i = if (length s) == 1 then (i+digitToInt (head s))
             else getInt (tail s) (digitToInt (head s)*10)+i*10

getPokemonAtIndex :: [Pokemon] -> Int -> Pokemon
getPokemonAtIndex pokemons index = pokemons !! index

getPokemonFromString :: [Pokemon] -> String -> Pokemon
getPokemonFromString pokemons s = getPokemonAtIndex pokemons (getInt s 0 - 1)

getCandyIndex :: [Candy] -> Int -> Candy
getCandyIndex candies index = candies !! index

getCandyFromString :: [Candy] -> String -> Candy
getCandyFromString candies s = getCandyIndex candies (getInt s 0 - 1)

getCandyId :: [Candy] -> String -> Int
getCandyId list candy = id_ (getCandyFromString list candy)

getPokemon :: [Pokemon] -> Int -> Pokemon
getPokemon (x:xs) id = if (candy_id x) == id then x
                        else getPokemon xs id

getPokemonFromCandyId :: [Pokemon] -> [Candy] -> String -> Pokemon
getPokemonFromCandyId poks candies canId = getPokemon poks (getCandyId candies canId)

evolutionOfPokemon :: [Pokemon] -> Pokemon -> Int
evolutionOfPokemon [] pok = -1
evolutionOfPokemon (x:xs) pok = if eatSameCandy pok x then ((id x) -1)
                                  else evolutionOfPokemon xs pok

eatSameCandy :: Pokemon -> Pokemon -> Bool
eatSameCandy pok1 pok2 = if ((candy_id pok1) == (candy_id pok2) && (id pok1 < id pok2)) then True
                        else False