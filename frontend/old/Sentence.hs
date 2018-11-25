{-# LANGUAGE OverloadedStrings #-}
module Sentence where

import           Control.Monad.Random

import qualified Data.IntMap as M
import           Data.Text (Text)
import qualified Data.Text as T

type Sentence = Text
splitIntoSentences :: Text -> [Sentence]
splitIntoSentences = filter (not . T.null) . map T.strip . T.splitOn "."

data MaskedSentence = Shown Sentence | Hidden Sentence
                    
isShown :: MaskedSentence -> Bool
isShown (Shown _) = True
isShown (Hidden _) = False

isHidden :: MaskedSentence -> Bool
isHidden = not . isShown

shownSize :: MaskedSentence -> Int
shownSize (Shown t) = T.length t
shownSize (Hidden _) = 0

shownText :: MaskedSentence -> Sentence
shownText (Shown t) = t
shownText (Hidden _) = "[...]"

hide :: MaskedSentence -> MaskedSentence
hide (Shown t) = Hidden t
hide (Hidden t) = Hidden t

reveal :: MaskedSentence -> MaskedSentence
reveal (Shown t) = Shown t
reveal (Hidden t) = Shown t

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

revealSentence :: MonadRandom m => [MaskedSentence] -> m [MaskedSentence]
revealSentence sentences = do
  let sen = (M.fromList (zip [0..] sentences))
      hid = M.filter isHidden sen
  if length hid == 0
  then return sentences
  else do
    s <- getRandomR ((0,length hid - 1)::(Int,Int))
    return $ M.elems (M.adjust reveal (M.keys hid !! s) sen)

composeSentences :: [Sentence] -> Sentence
composeSentences = T.intercalate ". "
