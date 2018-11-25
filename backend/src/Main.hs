{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Prelude hiding (error,id)
import           Types

import           Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import qualified Control.Monad.Logger as L
import           Control.Monad.IO.Class

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as T

import qualified Crypto.Random as R
import qualified Crypto.Hash as H

import           Data.Text(Text)
import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.HVect (HVect(..))
import           Data.ByteArray as BA

import           Database.Persist.Sqlite hiding (get)
import qualified Database.Persist as P

import           Web.Spock
import           Web.Spock.Config
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           System.Exit (die)

data AppSession = AppSession { user :: Maybe User }
data AppState = AppState { random :: TVar R.ChaChaDRG }

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "animals.db" 5
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runStdoutLoggingT $ runSqlPool (updateWhere [UserName ==. "sven"] [UserEnabled =. True]) pool
  drg <- R.drgNew
  gen <- T.atomically $ T.newTVar drg
  let session = AppSession { user = Nothing }
      state   = AppState { random = gen }
  spockCfg <- defaultSpockCfg session (PCPool pool) state
  let cfg = spockCfg
  runSpock 8080 $ (logStdoutDev.) <$> (spock cfg app)

app :: SpockM SqlBackend AppSession AppState ()
app = prehook initHook $ do

  get ("animal" <//> var) $ \animalId -> do
    (m :: Maybe Animal) <- runSQL $ P.get animalId
    case m of
      Nothing     -> do
        setStatus status404
        error "no animal with this id found"
      Just animal -> json animal

  get "animals" $ do
    animals <- runSQL $ selectList [] []
    json $ do
      animalEntity <- animals
      let animal = entityVal animalEntity
      return $ A.object
        [ "id" .= entityKey animalEntity
        , "popularity" .= animalPopularity animal
        , "taxonomy" .= animalTaxonomy animal
        ]

  post "signup" $ do
    usr :: User <- jsonBody'
    state <- getState
    salt <- liftIO $ T.atomically $ do
      rng <- T.readTVar (random state)
      let (salt,rng') = R.randomBytesGenerate 10 rng
      T.writeTVar (random state) rng'
      return salt
    let usr' = User
         { userName = userName usr
         , userPassword = sha3_512 (userPassword usr <> salt)
         , userSalt = salt
         , userEnabled = False
         }
    id <- runSQL $ insert usr'
    setStatus status201
    json $ A.object
      [ "result" .= A.String "success"
      , "id" .= id
      ]

  get "signin" $ do
    usr :: User <- jsonBody'
    m <- runSQL $ selectFirst [UserName ==. userName usr] []
    case m of
      Just (entityVal -> usr')
        | sha3_512 (userPassword usr <> userSalt usr') == userPassword usr' && userEnabled usr' -> do
           sessionRegenerateId
           writeSession (AppSession {user = Just usr'})
           json $ A.object [ "result" .= A.String "success" ]
        | otherwise -> do
           setStatus status401
           error "wrong input credentials"
      Nothing -> do
        setStatus status404
        error "User with this name does not exist"

  prehook authHook $ do
    post "animal" $ do
      animal :: Animal <- jsonBody'
      id <- runSQL $ insert animal
      setStatus status201
      json $ A.object
        [ "result" .= A.String "success"
        , "id" .= id
        ]

    post ("animal" <//> var) $ \animalId -> do
      animal :: Animal <- jsonBody'
      runSQL $ replace animalId animal
      setStatus status200
      json $ A.object [ "result" .= A.String "success" ]

    prehook adminHook $ do
      post ("user" <//> "enable" <//> var) $ \name -> do
        usr <- runSQL $ selectFirst [UserName ==. name] []
        case usr of
          Just _ -> do
            setStatus status200
            runSQL $ updateWhere [UserName ==. name] [UserEnabled =. True]
            json $ A.object [ "result" .= A.String "success" ]
          Nothing -> do
            setStatus status404
            error "User with this name does not exist"


sha3_512 :: ByteString -> ByteString
sha3_512 = BA.convert . H.hashWith H.SHA3_512

authHook :: ActionCtxT (HVect xs) (WebStateM conn AppSession t) (HVect (User ': xs))
authHook = do
  oldCtx <- getContext
  session <- readSession
  case user session of
    Nothing -> do
      setStatus status401
      error "not signed in"
    Just u -> return (u :&: oldCtx)

data Admin = Admin
adminHook :: ActionCtxT (HVect (User ': xs)) (WebStateM conn AppSession t) (HVect (Admin ': User ': xs))
adminHook = do
  usr :&: oldCtx <- getContext
  case userName usr of
    "sven" -> return (Admin :&: usr :&: oldCtx)
    _ -> do
      setStatus status401
      error "no admin rights: permission denied"

initHook :: Monad m => ActionCtxT () m (HVect '[])
initHook = return HNil

type ApiAction c a = SpockActionCtx c SqlBackend AppSession AppState a

error :: MonadIO m => Text -> ActionCtxT ctx m a
error message = do
  json $ A.object
    [ "result" .= A.String "failure"
    , "error" .= A.object ["message" .= A.String message]
    ]

loadAnimals :: IO ()
loadAnimals = do
  contents <- B.readFile "animals.json"
  animals <- case A.eitherDecode contents of
    Right as -> return (as :: [Animal])
    Left msg -> die $ "could not decode animals.json: " ++ msg
  _ <- runSqlite "animals.db" $ do
    runMigration migrateAll
    insertMany animals
  return ()

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
