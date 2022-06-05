{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
    ( file,
      get,
      html,
      param,
      post,
      raise,
      redirect,
      rescue,
      scotty,
      setHeader,
      status,
      text, notFound )

import Network.HTTP.Types (status302)
import Look ( startpage,
    showPokemons,
    startBattle,
    getEvolution,
    loadError,
    showWinner,
    allBattles,
    showCandies,
    evolutionResults,
    candyConsumer)
import Text.Blaze.Html.Renderer.Text ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Database (deleteBattle)

main :: IO ()
main = do
  scotty 3000 $ do
    get "/" $ do
        startpage        

    get "/pokemons" $ do
        name <- param "pokemontype" `rescue` (\_ -> return "All")
        showPokemons name

    get "/battle" $ do
        p1 <- param "pok1" `rescue` (\_ -> return "0")
        p2 <- param "pok2" `rescue` (\_ -> return "0")
        if p1 == "0" && p2 =="0" then startBattle
        else showWinner p1 p2

    get "/allBattles" $ do
        num <- param "num" `rescue` (\_ -> return "0")
        if num == "0" then allBattles "0"
        else allBattles num 

    get "/candies" $ do
        num <- param "num" `rescue` (\_ -> return "0")
        if num == "0" then showCandies
        else candyConsumer num

    get "/evolution" $ do
        num <- param "num" `rescue` (\_ -> return "0")
        if num == "0" then getEvolution
        else evolutionResults num

    get "/css" $ do
        file "src/util/style.css"

    get "/backgrounds/1" $ do
        file "src/util/1.jpg"
    
    get "/backgrounds/2" $ do
        file "src/util/2.jpg"

    get "/backgrounds/3" $ do
        file "src/util/3.jpg"
    
    get "/backgrounds/4" $ do
        file "src/util/4.jpg"

    get "/backgrounds/5" $ do
        file "src/util/5.jpg"

    get "/cursors/1" $ do
        file "src/util/bulbasaur.cur"

    get "/cursors/2" $ do
        file "src/util/squirtle.cur"

    get "/util/winner" $ do
        file "src/util/winner.png"
        
    get "/util/sad" $ do
        file "src/util/sad.png"
    
    notFound $ do
        loadError
