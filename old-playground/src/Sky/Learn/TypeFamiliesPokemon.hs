
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sky.TypeFamiliesPokemon where


class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
    data Move pokemon :: *
    pickMove :: pokemon -> Move pokemon

data Fire = Charmander | Charmeleon | Charizard deriving Show
instance Pokemon Fire where
    data Move Fire = Ember | FlameThrower | FireBlast deriving Show
    pickMove :: Fire -> Move Fire
    pickMove Charmander = Ember
    pickMove Charmeleon = FlameThrower
    pickMove Charizard = FireBlast

data Water = Squirtle | Wartortle | Blastoise deriving Show
instance Pokemon Water where
    data Move Water = Bubble | WaterGun deriving Show
    pickMove :: Water -> Move Water
    pickMove Squirtle = Bubble
    pickMove _ = WaterGun

data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
instance Pokemon Grass where
    data Move Grass = VineWhip deriving Show
    pickMove :: Grass -> Move Grass
    pickMove _ = VineWhip

----------------------------------------------------------------------------------------------------

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
  putStrLn $ pokemonOne ++ " used " ++ moveOne
  putStrLn $ pokemonTwo ++ " used " ++ moveTwo
  putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Pokemon pokemon, Pokemon foe, Show (Winner pokemon foe)) => Battle pokemon foe where
    type Winner pokemon foe :: *
    type Winner pokemon foe = pokemon

    pickWinner :: pokemon -> foe -> (Winner pokemon foe)
    -- pickWinner pokemon foe = pokemon

    battle :: pokemon -> foe -> IO ()
    battle pokemon foe = do
            printBattle (show pokemon) (show move) (show foe) (show foeMove) (show winner)
        where
            move :: (Pokemon pokemon) => Move pokemon
            move = pickMove pokemon
            foeMove :: (Pokemon foe) => Move foe
            foeMove = pickMove foe
            winner :: Winner pokemon foe
            winner = pickWinner pokemon foe

instance Battle Water Fire where
    pickWinner pokemon foe = pokemon

instance Battle Fire Water where
    type Winner Fire Water = Water
    pickWinner = flip pickWinner

instance Battle Grass Water where
    pickWinner pokemon foe = pokemon

instance Battle Water Grass where
    type Winner Water Grass = Grass
    pickWinner = flip pickWinner

instance Battle Fire Grass where
    pickWinner pokemon foe = pokemon

instance Battle Grass Fire where
    type Winner Grass Fire = Fire
    pickWinner = flip pickWinner

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  battle Squirtle Charmander
  battle Charmeleon Wartortle
  battle Bulbasaur Blastoise 
  battle Wartortle Ivysaur
  battle Charmeleon Ivysaur
  battle Venusaur Charizard
