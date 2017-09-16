{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TierQuiz where

import           Control.Monad.Random

import           Data.Aeson (FromJSON,ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import           Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.IntMap as M

import           GHC.Generics

import           System.Exit (die)
import           System.Random (randomRIO)

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

type Sentence = Text
splitIntoSentences :: Text -> [Sentence]
splitIntoSentences = filter (not . T.null) . map T.strip . T.splitOn "."

data MaskedSentence = Shown Sentence | Hidden Sentence
                    
isShown :: MaskedSentence -> Bool
isShown (Shown _) = True
isShown (Hidden _) = False

shownSize :: MaskedSentence -> Int
shownSize (Shown t) = T.length t
shownSize (Hidden _) = 0

shownText :: MaskedSentence -> Sentence
shownText (Shown t) = t
shownText (Hidden _) = "[...]"

hide :: MaskedSentence -> MaskedSentence
hide (Shown t) = Hidden t
hide (Hidden t) = Hidden t

mapText :: (Text -> Text) -> MaskedSentence -> MaskedSentence
mapText f (Shown t) = Shown (f t)
mapText f (Hidden t) = Hidden (f t)

type Percent = Double
removeSentences :: MonadRandom m => Percent -> [Sentence] -> m [MaskedSentence]
removeSentences p sentences = M.elems <$> loop (M.fromList (zip [0..] (map Shown sentences)))
  where
    loop reduced
      | size reduced <= totalSize * p
        || M.size (M.filter isShown reduced) == 1 = return reduced
      | otherwise = do
          let red = M.filter isShown reduced
          s <- getRandomR ((0,length red - 1)::(Int,Int))
          loop (M.adjust hide (M.keys red !! s) reduced)
    totalSize = size (map Shown sentences)
    size ss = fromIntegral $ sum $ fmap shownSize ss

composeSentences :: [Sentence] -> Sentence
composeSentences = T.intercalate ". "

maskSpecies :: Animal -> Text -> Text
maskSpecies animal = replaceByDots $
  (species animal) ++ (map T.toLower (species animal))
 where
   replaceByDots :: [Text] -> Text -> Text
   replaceByDots rs t = foldr (\r -> T.replace r "...") t rs

type Difficulty = Int
guessSpecies :: Animal -> Difficulty -> IO ()
guessSpecies animal difficulty = loop
  [ ("Verbreitung", distribution animal)
  , ("Habitat", habitat animal)
  , ("Ernährung", diet animal)
  , ("Sozialstruktur", socialStructure animal)
  , ("Fortpflanzung", reproduction animal)
  , ("Verhalten", behavior animal)
  , ("Merkmale", characteristic animal)
  , ("Systematik", T.pack (show (taxonomy animal)))
  , ("Bild", image animal)
  ]
  where
    percent = fromIntegral (10 - (difficulty - 1)) / 10

    loop attrs = case attrs of
      ((attr,txt):rest)
        | T.null txt -> loop rest
        | otherwise -> do
            T.putStr "\n### "
            T.putStr attr
            T.putStr " ###\n"
            txt' <- removeSentences percent (splitIntoSentences txt)
            T.putStrLn (maskSpecies animal (composeSentences (map shownText txt')))
            T.putStrLn ""
            guessOrTip rest
      [] ->
        T.putStrLn $ "Es gibt keine weiteren Hinweise mehr. Die Antwort lautet "
          `T.append` head (species animal)

    guessOrTip attrs = do
      T.putStrLn "(R)ate die Tierart oder noch einen (H)inweis"
      action <- T.getLine
      case toLower $ T.head action of
        'r' -> do
          T.putStrLn "Gib den Namen des Tieres ein:"
          guess <- T.getLine
          if guess `elem` species animal
             then T.putStrLn "Glückwunsch, deine Antwort ist richtig"
             else do
               T.putStrLn "Tut mir leid, deine Antwort ist falsch. Probier es noch einmal"
               guessOrTip attrs
        'h' -> do
           loop attrs
        _ -> guessOrTip attrs

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
