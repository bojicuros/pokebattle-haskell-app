{-# LANGUAGE OverloadedStrings #-}

module Look
  ( startpage,
    showPokemons,
    startBattle,
    getEvolution,
    loadError,
    showWinner,
    allBattles,
    showCandies,
    evolutionResults,
    candyConsumer
  )
where
  
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.String (fromString)
import Database
import Text.Blaze.Html.Renderer.Pretty as P (renderHtml)
import Text.Blaze.Html.Renderer.Text as T (renderHtml)
import Text.Blaze.Html5 as H (Html, a,b, body, br, div, docTypeHtml, form, h1, h2, head, html, img, input, label, li, link, option, p, select, span, title, toHtml, ul, (!))
import Text.Blaze.Html5.Attributes as A (action, alt, class_, for, href, id, method, name, rel, src, type_, value, required)
import Text.Blaze.Internal as I
import Types as Ty (Pokemon (..),Battle(..),Candy(..))
import PokeTypes as Pt
import Web.Scotty as S (html, text)
import qualified Web.Scotty
import System.IO.Unsafe (unsafePerformIO)

allPokemons :: [Pokemon]
allPokemons = unsafePerformIO getAllPokemons

candyList :: [Candy]
candyList = unsafePerformIO getAllCandies

battles :: [Battle]
battles = unsafePerformIO getAllBattles

battlesAfterDeletion :: String -> [Battle]
battlesAfterDeletion s = unsafePerformIO (deleteBattle (Pt.getInt s 0))

startpage :: Web.Scotty.ActionM ()
startpage = S.html . T.renderHtml $ do
  H.head $ do
    H.title "START PAGE"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina1" $ do
      H.form ! action "/pokemons?pokemontype=All" ! class_ "forma" $ do
        H.input ! A.type_ "submit" ! class_ "button button1" ! value "SHOW POKEMONS"
      H.form ! action "/battle" ! class_ "forma" $ do
        H.input ! A.type_ "submit" ! class_ "button button2" ! value "BATTLE"
      H.form ! action "/candies" ! class_ "forma" $ do
        H.input ! A.type_ "submit" ! class_ "button button3" ! value "CANDIES"
      H.form ! action "/evolution" ! class_ "forma" $ do
        H.input ! A.type_ "submit" ! class_ "button button4" ! value "EVOLUTION"

allPokeTypes :: [String]
allPokeTypes = ["All", "Grass", "Poison", "Fire", "Ice", "Flying", "Psychic", "Water", "Ground", "Rock", "Electric", "Bug", "Normal", "Dragon", "Fairy", "Dark", "Fighting", "Ghost", "Steel"]

showPokemons :: String -> Web.Scotty.ActionM ()
showPokemons tip= S.html . T.renderHtml $ do
  H.head $ do
    H.title "POKEMONS"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina2" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "POKEMON TYPE" ! A.for "type"
        renderOptions
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "SHOW"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "pokelist background1" $ do
        if tip=="All" then renderPokemons allPokemons 1
        else renderPokemons (specialPokemon tip) 1
  
specialPokemon :: String -> [Pokemon]
specialPokemon tip = filter (\x ->Pt.haveType tip x) allPokemons

renderOptions :: Html
renderOptions = do
  H.select ! A.id "pokemontype" ! A.name "pokemontype" $ forM_ allPokeTypes (H.option . toHtml)

renderPokemons :: [Pokemon] -> Int -> Html
renderPokemons xs a = do
  forM_ xs (onePokemon a)

onePokemon :: Int -> Pokemon -> Html
onePokemon a pok =
  if a == 1
    then H.div ! A.class_ "pokeshow" $ do
      H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img pok))
      H.p (I.string (spoji "Name:" (Ty.name pok))) ! A.class_ "text"
      H.p (I.string (spoji "Height:" (Ty.height pok))) ! A.class_ "text"
      H.p (I.string (spoji "Width:" (Ty.weight pok))) ! A.class_ "text"
      H.p (I.string (spoji "Type:" (Ty.type_ pok))) ! A.class_ "text"
      H.p (I.string (spoji "Weaknesses:" (Ty.weakness pok))) ! A.class_ "text"
    else H.div ! A.class_ "pokeshow" $ do
      H.p (I.string (spoji (show (Ty.id pok)) ".")) ! A.class_ "text redniBroj"
      H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img pok))
      H.p (I.string (spoji "Name:" (Ty.name pok))) ! A.class_ "text"
      H.p (I.string (spoji "Height:" (Ty.height pok))) ! A.class_ "text"
      H.p (I.string (spoji "Width:" (Ty.weight pok))) ! A.class_ "text"
      H.p (I.string (spoji "Type:" (Ty.type_ pok))) ! A.class_ "text"
      H.p (I.string (spoji "Weaknesses:" (Ty.weakness pok))) ! A.class_ "text"

spoji :: String -> String -> String
spoji s1 s2 = unwords [s1, s2]

startBattle :: Web.Scotty.ActionM ()
startBattle = S.html . T.renderHtml $ do
  H.head $ do
    H.title "BATTLE"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina3" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "First pokemons num:" ! A.for "num"
        H.input ! A.name "pok1" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.label "Secound pokemons num:" ! A.for "num"
        H.input ! A.name "pok2" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "CONFIRM"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "pokelist background2" $ do
        renderPokemons allPokemons 2

showWinner :: String -> String -> Web.Scotty.ActionM ()
showWinner p1 p2= S.html . T.renderHtml $ do
  H.head $ do
    H.title "BATTLE"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina3" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "First pokemons num:" ! A.for "num"
        H.input ! A.name "pok1" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.label "Secound pokemons num:" ! A.for "num"
        H.input ! A.name "pok2" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "CONFIRM"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "pokelist background2" $ do
        renderPokemons allPokemons 2
        addWinner p1 p2

addWinner :: String -> String -> Html
addWinner p1 p2 =H.div ! class_ "cursor centered-div" $ do
                    H.form ! class_ "menu menuOnDiv" $ do
                      H.a ! href "/battle" $ do
                        H.input ! A.class_ "button" ! A.type_ "button" ! A.value "CLOSE"
                      H.a ! href "/allBattles" $ do
                        H.input ! A.class_ "button" ! A.type_ "button" ! A.value "ALL BATTLES"
                      H.div ! class_ "results" $ do
                        H.img ! A.src (I.stringValue (Ty.img (allPokemons !! ((Ty.winn ((afterAdition allPokemons p1 p2))) -1))))
                        H.img ! A.class_ "otherimg" ! A.src ("/util/winner")

afterAdition :: [Pokemon] -> String -> String -> Battle
afterAdition xs p1 p2 =last(unsafePerformIO (addStronger xs p1 p2))

allBattles :: String -> Web.Scotty.ActionM ()
allBattles index= S.html . T.renderHtml $ do
  H.head $ do
    H.title "BATTLE"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina3" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "Delete battle that is under number:" ! A.for "delete"
        H.input ! A.name "num" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "DELETE"
        H.a ! href "/battle" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.br
      H.b "   First pokemon" ! class_ "txt"
      H.b "  Secound pokemon" ! class_ "txt"
      H.b " Winner" ! class_ "txt"
      H.div ! A.class_ "battlelist background2" $ do
        if (index == "0" ) then renderBattleResults battles
        else renderBattleResults (battlesAfterDeletion index)

renderBattleResults :: [Battle] -> Html
renderBattleResults xs = do
  forM_ xs oneResult

oneResult :: Battle -> Html
oneResult btl =
    H.div ! A.class_ "battleshow" $ do
      H.p (I.string (spoji (show (Ty.iD btl)) ".")) ! A.class_ "text redniBroj"
      H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img (getPokemon ((Ty.frst btl)-1))))
      H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img (getPokemon ((Ty.secn btl)-1))))
      H.img ! A.class_ "pokewinner" ! A.src (I.stringValue (Ty.img (getPokemon ((Ty.winn btl)-1))))

getPokemon :: Int -> Pokemon
getPokemon num  = allPokemons !! num

showCandies :: Web.Scotty.ActionM ()
showCandies  = S.html . T.renderHtml $ do
  H.head $ do
    H.title "CANDIES"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina4" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "Show pokemon that consume candy:" ! A.for "show"
        H.input ! A.name "num" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "SHOW"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "candylist background5" $ do
        renderCandies candyList

renderCandies :: [Candy] -> Html
renderCandies xs = do
  forM_ xs getCandy

getCandy :: Candy -> Html
getCandy can =
  H.div ! A.class_ "candyshow" $ do
    H.p (I.string (spoji (show (Ty.id_ can)) ".")) ! A.class_ "text redniBroj2"
    H.img ! A.class_ "candyimg" ! A.src (I.stringValue (Ty.img_ can))
    H.br
    H.p (I.string (spoji "Name:" (Ty.name_ can))) ! A.class_ "text"
    H.p (I.string (spoji "Quantity:" (show (Ty.quantity can)))) ! A.class_ "text"

candyConsumer :: String -> Web.Scotty.ActionM ()
candyConsumer candyIndxStr = S.html . T.renderHtml $ do
  H.head $ do
    H.title "CANDIES"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina4" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "Show pokemon that consume candy:" ! A.for "show"
        H.input ! A.name "num" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "SHOW"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "candylist background5" $ do
        renderCandies candyList
        addPokeConsumer candyIndxStr (Pt.getPokemonFromCandyId allPokemons candyList candyIndxStr)

addPokeConsumer :: String -> Pokemon -> Html
addPokeConsumer candy pok =H.div ! class_ "centered-div3" $ do
                    H.form ! class_ "menu menuOnDiv" $ do
                      H.a ! href "/candies" $ do
                        H.input ! A.class_ "button" ! A.type_ "button" ! A.value "CLOSE"
                      H.div ! class_ "results" $ do
                        H.img ! A.class_ "candyImg" ! A.src (I.stringValue (Ty.img_ (Pt.getCandyFromString candyList candy)))
                        H.b "Candy is consumed by:" ! class_ "redniBroj2"
                        H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img pok))


getEvolution :: Web.Scotty.ActionM ()
getEvolution = S.html . T.renderHtml $ do
  H.head $ do
    H.title "EVOLUTION"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina5" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "Show evolution of pokemon who's ordinal number is:" ! A.for "num"
        H.input ! A.name "num" ! A.class_ "unosBroja text" ! A.type_ "text" ! A.required ""
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "SHOW"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "pokelist background4" $ do
        renderPokemons allPokemons 2

evolutionResults :: String -> Web.Scotty.ActionM ()
evolutionResults indexStr=  S.html . T.renderHtml $ do
  H.head $ do
    H.title "EVOLUTION"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina5" $ do
      H.form ! class_ "menu" ! method "GET" $ do
        H.label "Show evolution of pokemon who's ordinal number is:" ! A.for "num"
        H.input ! A.name "num" ! A.class_ "unosBroja text" ! A.type_ "text"
        H.input ! A.class_ "button" ! A.type_ "submit" ! A.value "SHOW"
        H.a ! href "/" $ do
          H.input ! A.class_ "button" ! A.type_ "button" ! A.value "BACK"
      H.div ! A.class_ "pokelist background4" $ do
        renderPokemons allPokemons 2
        let pok = Pt.getPokemonFromString allPokemons indexStr
        getSpecificResult pok (Pt.evolutionOfPokemon allPokemons pok)

getSpecificResult :: Pokemon -> Int -> Html
getSpecificResult pok num2 =
  if num2>0    --ima evoluciju
    then H.div ! class_ "cursor centered-div2" $ do
                    H.form ! class_ "menu menuOnDiv" $ do
                      H.a ! href "/evolution" $ do
                        H.input ! A.class_ "button" ! A.type_ "button" ! A.value "CLOSE"
                      H.div ! class_ "results" $ do
                        H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img pok))
                        H.b "Evolues to" ! class_ "redniBroj"
                        H.img ! A.class_ "pokeimg" ! A.src (I.stringValue (Ty.img (Pt.getPokemonAtIndex allPokemons num2)))
    else H.div ! class_ "cursor centered-div2" $ do  --nema evoluciju
                    H.form ! class_ "menu menuOnDiv" $ do
                      H.a ! href "/evolution" $ do
                        H.input ! A.class_ "button" ! A.type_ "button" ! A.value "CLOSE"
                      H.div ! class_ "results" $ do
                        H.b "   Sorry, this pokemon has no" ! class_ "redniBroj"
                        H.br
                        H.b "   further evolutions" ! class_ "redniBroj"
                        H.img ! A.class_ "otherimg" ! A.src ("/util/sad")

loadError :: Web.Scotty.ActionM ()
loadError = S.html . T.renderHtml $ do
  H.head $ do
    H.title "404 ERROR"
    link ! rel "stylesheet" ! A.type_ "text/css" ! A.href "/css"
  body $ do
    H.div ! class_ "cursor pozadina6" $ do
      H.label "PAGE NOT FOUND" ! A.class_ "naslov"

