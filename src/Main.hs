{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans
import qualified Data.Aeson as A
import           Data.Monoid
import           Data.Time.Clock (NominalDiffTime)
import           Data.Text (Text, pack)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString,
                                    runSqlPool, getBy, insert, selectList, Entity(..), (==.),
                                    fromSqlKey)
import qualified Database.Persist.Sql as Sql
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Status
import           Web.Spock.Safe
import           Web.Users.Persistent
import           Web.Users.Types
import Data.HVect hiding (pack)
import Control.Monad

import           Models

type AppAction ctx sess = SpockActionCtx ctx Sql.SqlBackend sess AppState

data AppState = AppState { userBackend :: Persistent
                         }

main :: IO ()
main = do
  pool <- runNoLoggingT $ createPostgresqlPool connStr 3
  runSqlPool doMigrations pool
  let backend = Persistent $ flip runSqlPool pool
  initUserBackend backend
  runSpock 8080 $ spock (spockCfg pool backend) countableApp

connStr = "host=localhost dbname=countable user=develop password=develop port=5432"

data LoginReq = LoginReq { username :: Text
                         , password :: Text
                         } deriving (Show, Generic)

instance A.FromJSON LoginReq

data NewCounter = NewCounter { name :: String
                             } deriving (Show, Generic)

instance A.FromJSON NewCounter

sessionDur :: NominalDiffTime
sessionDur = 60 * 60 -- An hour

authenticateSession :: AppAction ctx sess (UserId Persistent)
authenticateSession = do
  maybeSessionId <- header "Authentication"
  case maybeSessionId of
    Just sessionId -> do
      maybeUserId <- runUser (\b -> verifySession b (SessionId sessionId) sessionDur)
      case maybeUserId of
        Nothing -> text "Invalid session token provided"
        Just userId -> return userId
    Nothing -> do setStatus status401
                  text "No session token provided"

initHook :: (Monad m) => ActionCtxT () m (HVect '[])
initHook = return HNil

authHook :: AppAction (HVect xs) sess (HVect (UserId Persistent ': xs))
authHook = do oldCtx <- getContext
              user <- authenticateSession
              return (user :&: oldCtx)

getUserId :: ListContains n (UserId Persistent) xs => AppAction (HVect xs) sess (UserId Persistent)
getUserId = do (userId :: UserId Persistent) <- liftM findFirst getContext
               return userId

countableApp :: SpockCtxM () Sql.SqlBackend ses AppState ()
countableApp = 
  prehook initHook $ do
    -- Users
    post ("api/users") $ do -- Create user
      user <- jsonBody'
      res <- runUser (\b -> createUser (b :: Persistent) (user :: User String))
      case res of
        Right id -> do
            setStatus status200
            text $ pack . show . fromSqlKey $ (id :: UserId Persistent)
        Left err -> do
            setStatus status500
            text . pack . show $ err

    post ("api/login") $ do -- Login user and return session id
      login <- jsonBody'
      session <- runUser (\b -> authUser b (username login) (PasswordPlain . password $ login) sessionDur)
      text $ maybe "Authentication failed" unSessionId session

    -- Counters
    prehook authHook $
      do get "api/counters" $ do
           userId <- getUserId
           counters <- runDb $ selectList [CounterUserId ==. userId] []
           json (counters :: [Entity Counter])

         post "api/counters" createCounterAction

         get ("api/counters" <//> var) $ \counterId ->
           do userId <- getUserId
              maybeCounter <- runDb $ Sql.get (Sql.toSqlKey counterId)
              case maybeCounter of
                Just counter -> do
                  if (counterUserId counter) == userId
                     then json (counter :: Counter)
                     else setStatus status401 >> text ""
                Nothing -> do
                  setStatus status404
                  text "Counter not found"

         get ("api/counters" <//> var <//> "counts") $ \counterId ->
           do userId <- getUserId
              counts <- runDb $ selectList [CountCountedForId ==. counterId] []
              json (counts :: [Entity Count])

createCounterAction :: AppAction (HVect '[LoginId]) sess c
createCounterAction = do
    userId <- getUserId
    newCounter <- jsonBody'
    let counter = Counter (Main.name (newCounter :: NewCounter)) userId
    counterId <- runDb $ insert (counter :: Counter)
    setStatus status200
    text $ pack . show . fromSqlKey $ counterId

runUser :: (Persistent -> IO a) -> AppAction ctx foo a
runUser action = do
  state <- getState
  liftIO $ action (userBackend state)

spockCfg pool ub = defaultSpockCfg Nothing (PCPool pool) (AppState ub)
