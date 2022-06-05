# Projektni zaadatak iz predmeta funkcionalno programiranje

Ovo je opis **web-aplikacije** za projekat iz predmeta **Funkcionalno programiranje**. Zadatak nam je bio realizovati web-aplikaciju u programskom jeziku Haskel, tako što ćemo se uz pomoć Haskela povezati na bazu podataka i prikazati podatke iz iste. Podatke trebamo prikazati tako što ćemo generisati odgovarajući HTML kod, pored prikazivanja aplikacija treba da bude interaktivna. Potrebno je da omogućimo dodavanje podataka u bazu, te brisanje podataka iz baze. Takođe je potrebno uspostaviti komunikaciju između web aplikacije i servera na kom se ona izvršava.

Projekat je realizovan koristeći **SQLite** bazu podataka, a za konekciju sa bazom se koristi **sqlite-simple** bibliotaka. Generisanje HTML koda vrši se uz pomoć **blaze-html** biblioteke. Server je realizovan korisreći **scotty**, koji je zasnovan na *wai* interfejsu za web aplikacije i *warp* serveru za WAI aplikacije. 



Naziv web aplikacije je **Pokemon info** i ova aplikacija nam pruža mogućnost prikaza osnovnih informacija o pokemonima i o hrani koju oni konzumiraju. Pored toga, korisnik aplikacije će moći da simulira borbe pokemona, te da provjeri koji je jači, te da sazna informacije o onom što je ključno za svakog pokemona, a to je evolucija. Prilikom pokretanja aplikacije nudi nam se da izaberemo jednu od 4 opcije, a to su: 
1. Prikaz pokemona i osnovnih informacija o pokemonima
2. Simulacija borbe pokemona, kao i pregled borbi koje su ranije izvođene
3. Prikaz slatkiša koji pokemoni jednu i osnove informacije o njima
4. Prikaz pokemona i informacija o evoluciji svakog od pokemona

Izborom prve opcije prikazuje nam se slike svakog od pokemona, kao i osnove informacije (naziv,visina,težina,tip,slabosti). Na ovom ekranu imamo opciju izbora određenog tipa, te nam se tada prikazuju samo pokemoni čiji je tip ili jedan od tipova onaj koji je prethodno izabran.

Izborom druge opcije prikazuju nam se slike pokemona, osnovne informacije i redni broj pokemona. Nudi nam se opcija da unesemo dva redna broja, te se vrši simulacija borbe i prikazuje pobjednik. Prilikom prikaza pobjednika nudi nam se i opcija pomoću koje ćemo moći pogledati detaljnije informacije o prethodno izvršenoj borbi, ali i o svim borbama koje su ranije izvršene.

Izborom treće opcije prikazuje nam se redni broj i slika svakog od slatkiša, pored čega se nalazi i naziv i dostupna količina slatkiša. Nudi nam se opcija za unos rednog broja slatkiša, te ukoliko to učinimo prikazuje nam se slika pokemona koji konzumira taj slatkiš.

Izborom druge opcije prikazuju nam se slike pokemona, osnovne informacije i redni broj pokemona. Ukoliko unesemo redni broj pokemona vraća nam se informacija o evoluciji pokemona. Ukoliko pokemon može da evoluira vraća nam se u kog pokemona on evoluira, a ukoliko ne može tada nam se vraća informacija da pokemon nema dalje evolucije.


### Baza podataka

Baza podataka se naziva *pokedex.db* i sastoji se od tri tabele,**pokemon**, **candies** i **battle**. U prvoj tabeli nalaze se osnovne informacije o pokemonima. Primarni ključ je *id*, a strani ključ je *candy_id* (primarni ključ u tabeli candies). Prikaz par informacija iz tabele:

| id      | name      | img      | type      | height  | weight    | candy_id | weakness  |
| ------- | --------- | -------  | --------- | ------- | --------- | -------  | --------- |
| 1       | Bulbasaur |[http://www.serebii](https://www.serebii.net/pokemongo/pokemon/001.png)...|Grass,Poison|0.71 m|6.9 kg|1|Fire,Ice,Flying|
| 7       | Squirtle  |[http://www.serebii](https://www.serebii.net/pokemongo/pokemon/007.png)...|Water       |0.51 m|9.0 kg|3|Electric,Grass|
| 13       | Weedle  |[http://www.serebii](https://www.serebii.net/pokemongo/pokemon/013.png)...|Bug,Poison |0.30 m|3.2 kg|5|Fire,Flying,Rock|


U drugoj tabeli nalaze se informacije o slatkišima i primarni ključ je *id*, prikaz par informacija iz ove tabele:

| id      | name      | quantity | img      |
| ------- | --------- | -------  | --------- |
| 1       | Bulbasaur Candy |13    |https://static.wikia.nocookie.net/pokemongo/images/8/87/Bulbasaur_candy.png|
| 7       | Squirtle Candy  |68    |https://static.wikia.nocookie.net/pokemongo/images/8/85/Squirtle_candy.png|


U trećoj tabeli nalaze se informacije o borbama. Primarni ključ je *id*, a *firstId*,*secoundId* i *winnerId* predstavljaju sekundarni ključ. Prikaz par infromacija iz tabele:

| id      | firstId | secoundId | winnerId |
| ------- | --------- | -------  | --------- |
| 1 | 6 | 13 |6|
| 2 | 15 | 67 | 67 |
| 5 | 12 | 135 | 12 |

### [Konekcija sa bazom: sqlite-simple](https://hackage.haskell.org/package/sqlite-simple)

Biblioteka za konekciju sa bazom je sqlite-simple. Ova biblioteka omogućava da pišemo upite u čistom SQL kodu, što predstavlja prednost za programere koju poznaju SQL jer im štedi vrijeme potrebno za učenje sintakse.
Još jedna od prednosti ove biblioteke, ukoliko imamo definisan novi tip podataka, je to što možemo da iz baze učitamo direktno u taj tip podataka, odnosno taj tip podataka možemo direktno učitati u bazu. Ovo se realizuje pomoću tipova *FromRow* i *ToRow*.

```haskell
data Pokemon = Pokemon {  id :: Int,
                          name :: String,
                          img :: String,
                          type_ :: String,
                          height :: String,
                          weight :: String,
                          candy_id :: Int,
                          weakness :: String
                       }

instance FromRow Pokemon where
    fromRow = Pokemon <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Pokemon where
    toRow (T.Pokemon id name img type_ height weight candy_id weakness)
        = toRow (id,name,img,type_,height,weight,candy_id,weakness)

allPokemons :: IO [Pokemon]
allPokemons = do
        conn <- open "pokedex.db"
        query_ conn "SELECT * FROM pokemon" :: IO [Pokemon]

```

### [Generisanje HTML koda: blaze-html](https://hackage.haskell.org/package/blaze-html)

Biblioteka **BlazeHtml** se koristi da se iz koda koji se napisan u programskom jeziku Haskel generiše kod koji je napisan u opisnom jeziku HTML. Primjer:

Kod napisan u Haskelu:
```haskell 
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
```

Kod u HTML-u, generisan iz koda koji je napisan u Haskelu:
```html
<html>
 <head>
  <title>BATTLE</title>
  <link rel="stylesheet" href="style.css">
</head>

<body>
  <div class="cursor pozadina3">
    <form class="menu" method="GET">
      <label for="num">First pokemons num:</label>
      <input name="pok1" class="unosBroja text" type="text" required>
      <label for="num">Secound pokemons num:</label>
      <input name="pok2" class="unosBroja text" type="text" required>
      <input class="button" type="submit" value="CONFIRM">
    </form> 
  </div>
</body>
```


### [Server: Scotty](https://hackage.haskell.org/package/scotty)

Za definisanje servera korišćena je biblioteka **Scotty**, koja je inspirisana bibliotekom Sinatra koja je napisana u funkcionalnom jeziku *Ruby*. Funkcionalnost ove biblioteke je zasnovan na [*wai*](https://hackage.haskell.org/package/wai) interfejsu za web aplikacije, koji omogućava protokol za komunikaciju između web aplikacije i web servera, kao i na [*warp*](https://hackage.haskell.org/package/warp) serveru za WAI aplikacije koje koriste HTTP protokole za komunikaciju. 

U prethodno napisanom kodu u HTML, vidimo primjer slanja GET zahtjeva. Ukoliko server podesimo na port 3000, tada ćemo unosom dva broja (npr. 2 i 3) u odovarajuća input polja poslati GET zahtjev na sljedeći način: http://localhost:3000/battle?pok1=2&pok2=3. 

Primjer podešavanja servera na port 3000 i obrada zahtjeva:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

main :: IO ()
main = do
  scotty 3000 $ do     

    get "/battle" $ do     
        p1 <- param "pok1" `rescue` (\_ -> return "0")
        p2 <- param "pok2" `rescue` (\_ -> return "0")
        if p1 == "0" && p2 =="0" then startBattle
        else showWinner p1 p2
```
gdje su startBattle i showWinner funkcije koje generišu HTML kod. 

Na server možemo dodati i lokalne fajlove, pa ih koristiti u sklopu web aplikacije. Na primjer:
```haskell
    get "/css" $ do               
        file "src/util/style.css"  

    get "/backgrounds/1" $ do
        file "src/util/1.jpg"
        
    get "/util/winner" $ do
        file "src/util/winner.png"
```

Prilikom učitavanja web adrese server provjerava da li se adresa poklapa sa nekom od navedenih (npr. "/battle","/css","/backgrounds/1"), a ukoliko to nije slučaj prelazi se na 404 notFound.
```haskell
    notFound $ do      
        loadError
```
gdje je loadError funkcija koja generiše HTML kod, koji obavještava korisnika da data stranica ne postoji.


## Literatura

 - https://jaspervdj.be/blaze/tutorial.html.

 - https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation.

 - http://book.realworldhaskell.org/read/using-databases.html.

 - https://www.stackbuilders.com/blog/getting-started-with-haskell-projects-using-scotty/.

 - https://github.com/lucch/haskell-scotty-tutorial.

 - http://www.mchaver.com/posts/2017-06-28-sqlite-simple.html.