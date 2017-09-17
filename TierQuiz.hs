{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module TierQuiz where

import           Sentence

import           Control.Monad.Random

import           Data.Aeson (FromJSON,ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           GHC.Generics

import           System.Exit (die)

import           Text.Printf

type Species = [Text]
type Distribution = Text
type Habitat = Text
type Behavior = Text
type Characteristic = Text
type SocialStructure = Text
type Reproduction = Text
type Diet = Text

data Animal = Animal
  {
    species :: Species,
    image :: Text,
    taxonomy :: Taxonomy,
    distribution :: Distribution,
    habitat :: Habitat,
    behavior :: Behavior,
    characteristic :: Characteristic,
    socialStructure :: SocialStructure,
    diet :: Diet,
    reproduction :: Reproduction
  }
  deriving (Show,Generic)

instance FromJSON Animal
instance ToJSON Animal

type Order = Text
type Family = Text
data Taxonomy = Taxonomy
  { order :: Order
  , family :: Family
  } deriving (Show,Generic)

instance FromJSON Taxonomy
instance ToJSON Taxonomy

readAnimals :: IO [Animal]
readAnimals = do
  json <- B.readFile "animals.json"
  case A.eitherDecode json of
    Right animals -> return animals
    Left errMsg -> die $ "could not parse animals.json: " ++ (show errMsg)

maskSpecies :: Animal -> Text -> Text
maskSpecies animal = replaceByDots $
  (species animal) ++ (map T.toLower (species animal))
 where
   replaceByDots :: [Text] -> Text -> Text
   replaceByDots rs t = foldr (\r -> T.replace r "...") t rs


printAttribute :: Animal -> Text -> [MaskedSentence] -> IO ()
printAttribute animal attr txt = do
  T.putStr "\n### "
  T.putStr attr
  T.putStr " ###\n"
  T.putStrLn (maskSpecies animal (composeSentences (map shownText txt)))
  T.putStrLn ""

type Difficulty = Int
guessSpecies :: Animal -> Difficulty -> IO ()
guessSpecies animal difficulty = do
  attrs <- mapM (second (removeSentences percent . splitIntoSentences))
    [ ("Verbreitung", distribution animal)
    , ("Habitat", habitat animal)
    , ("Ernährung", diet animal)
    , ("Sozialstruktur", socialStructure animal)
    , ("Fortpflanzung", reproduction animal)
    , ("Verhalten", behavior animal)
    , ("Merkmale", characteristic animal)
    ]
  loop attrs
  where
    percent = fromIntegral (10 - (difficulty - 1)) / 10

    second :: Monad m => (a -> m b) -> (c,a) -> m (c,b)
    second f (c,a) = (c,) <$> f a

    loop :: [(Text,[MaskedSentence])] -> IO ()
    loop attrs = case attrs of
      ((attr,txt):rest)
        | null txt -> loop rest
        | otherwise -> do
            printAttribute animal attr txt
            guessOrTip attr txt rest
      [] ->
        T.putStrLn $ "Es gibt keine weiteren Hinweise mehr. Die Antwort lautet "
          `T.append` head (species animal)

    guessOrTip attr txt rest = do
      T.putStrLn "(R)ate die Tierart, noch einen (H)inweis, oder (m)ehr Hinweise in dieser Katerogie"
      action <- T.getLine
      case toLower $ T.head action of
        'r' -> do
          T.putStrLn "Gib den Namen des Tieres ein:"
          guess <- T.getLine
          if guess `elem` species animal
             then T.putStrLn "Glückwunsch, deine Antwort ist richtig"
             else do
               T.putStrLn "Tut mir leid, deine Antwort ist falsch. Probier es noch einmal"
               guessOrTip attr txt rest
        'h' -> do
           loop rest
        'm' -> do
          txt' <- revealSentence txt
          printAttribute animal attr txt'
          guessOrTip attr txt' rest
        _ -> guessOrTip attr txt rest

guessGame :: IO ()
guessGame = do
  animals <- readAnimals
  printf "Wähle ein Tier aus: %d - %d\n" (1::Int) (length animals)
  animalIndex <- read <$> getLine
  putStrLn "\nWähle eine Schwierigkeit aus: (1) leichteste Schwierigkeit - (10) schwerste Schwierigkeit"
  difficulty  <- read <$> getLine
  guessSpecies (animals !! (animalIndex - 1)) difficulty

main :: IO ()
main = do
  guessGame
