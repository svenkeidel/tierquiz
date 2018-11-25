{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Aeson (FromJSON,(.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Animal json
    species [Text]
    mask [Text] Maybe 
    popularity Int
    image Text
    taxonomy Taxonomy
    distribution Text
    habitat Text
    behavior Text
    characteristic Text
    socialStructure Text
    diet Text
    reproduction Text
    deriving Show Eq
  
  Taxonomy json
    kingdom Text Maybe 
    phylum Text Maybe
    _class Text Maybe         sql=class
    superorder Text Maybe
    order Text Maybe
    suborder Text Maybe
    infraorder Text Maybe
    superfamily Text Maybe
    family Text Maybe
    subfamily Text Maybe
    without_rank Text Maybe
    tribe Text Maybe
    genus Text Maybe 
    species Text Maybe
    deriving Show Eq
                                                                        
  User
    name Text
    password ByteString
    salt ByteString
    enabled Bool
    UniqueUser name
|]

instance FromJSON User where
  parseJSON (A.Object v) = User <$> v .: "name" <*> (encodeUtf8 <$> (v .: "password")) <*> pure B.empty <*> pure False
  parseJSON v = A.typeMismatch "User" v
