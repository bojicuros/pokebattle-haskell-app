{-# LANGUAGE OverloadedStrings #-}

module Database (
    getOrCreateCandy,
    queryCandyAllPokemons,
    queryLessThan,
    queryMoreThan,
    getAllPokemons,
    getAllCandies,
    getAllBattles,
    deleteBattle,
    addBattle
) where

import Types as T ( Candy (..), Pokemon (..) ,Pokemonrec(..), Battle(..))
import Control.Applicative ()
import Database.SQLite.Simple
    ( execute,
      open,
      queryNamed,
      field,
      FromRow(..),
      Connection,
      ToRow(..), NamedParam ((:=)), query, query_, close, Only(..))
import Database.SQLite.Simple.Internal ( Connection )
import Database.SQLite.Simple.FromRow ( field, FromRow(..) )
import Database.SQLite.Simple.ToRow ( ToRow(..) )
import Control.Exception ()


instance FromRow T.Pokemon where
    fromRow = T.Pokemon <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow T.Pokemon where
    toRow (T.Pokemon id name img type_ height weight candy_id weakness)
        = toRow (id,name,img,type_,height,weight,candy_id,weakness)


instance FromRow T.Candy where
    fromRow = T.Candy <$> field <*> field <*> field <*> field
instance ToRow T.Candy where
    toRow (T.Candy id_ name_ quantity img_)
        = toRow (id_, name_, quantity, img_)

instance FromRow T.Battle where
    fromRow = T.Battle <$> field <*> field <*> field <*> field
instance ToRow T.Battle where
    toRow (T.Battle iD frst secn winn)
        = toRow (iD, frst, secn, winn)

instance FromRow T.Pokemonrec where
    fromRow = T.Pokemonrec <$> field

initialiseDB :: IO Connection
initialiseDB = open "pokedex.db"

getAllPokemons :: IO [Pokemon]
getAllPokemons = do
        conn<-initialiseDB
        query_ conn "SELECT * FROM pokemon" ::IO [Pokemon]

getAllCandies :: IO [Candy]
getAllCandies = do
        conn<-initialiseDB
        query_ conn "SELECT * FROM candies" ::IO [Candy]

getAllBattles :: IO [Battle]
getAllBattles = do
        conn<-initialiseDB
        query_ conn "SELECT * FROM battle" ::IO [Battle]


addBattle :: Int -> Int -> Int -> IO [Battle] 
addBattle f s w= do
    conn<-initialiseDB
    execute conn "INSERT INTO battle (firstId,secoundId,winnerId) VALUES (?,?,?)" (f, s, w)
    query_ conn "SELECT * FROM battle" ::IO [Battle]


deleteBattle :: Int -> IO [Battle]
deleteBattle id= do
    conn<-initialiseDB
    execute conn "DELETE FROM battle WHERE id = ?" (Only id)
    query_ conn "SELECT * FROM battle" ::IO [Battle]

getEvoultion :: Int -> IO [Pokemon]
getEvoultion id = do
    conn<-initialiseDB
    queryNamed conn "SELECT * FROM pokemon JOIN pokemon AS p ON pokemon.id>p.id WHERE pokemon.candy_id=p.candy_id AND p.id=:temp LIMIT 1" [":temp" := id] :: IO [Pokemon]

getOrCreateCandy :: Connection -> String -> Int->IO Candy
getOrCreateCandy conn name quntity = do
    results <- queryNamed conn "SELECT * FROM candies WHERE name=:candy" [":candy" := name]
    if not (null results) then
        return . head $ results
    else do
        execute conn "INSERT INTO candies ( candy, candy_count) VALUES (?,?)" (name, quntity)
        getOrCreateCandy conn  name quntity

queryCandyAllPokemons :: Connection -> String ->IO [Pokemonrec]
queryCandyAllPokemons conn candyName= do
    let sql = "SELECT pokemon.name FROM pokemon INNER JOIN candies ON pokemon.candy_id=candies.id WHERE candies.name=?"
    query conn sql [candyName]

queryLessThan :: Connection -> IO [Pokemonrec]
queryLessThan conn = do
    putStr "Enter a number to see the pokemons with less candies than the entered number  "
    maxNum <- getLine
    let sql = "SELECT pokemon.name FROM pokemon INNER JOIN candies ON pokemon.candy_id=candies.id WHERE quantity< ?"
    query conn sql [maxNum]

queryMoreThan :: Connection -> IO [Pokemonrec]
queryMoreThan conn = do
    putStr "Enter a number to see the pokemons with more candies than the entered number  "
    minNum <- getLine
    let sql = "SELECT pokemon.name FROM pokemon INNER JOIN candies ON pokemon.candy_id=candies.id WHERE quantity> ?"
    query conn sql [minNum]

