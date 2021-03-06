{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad
import           Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans
import qualified Data.Aeson as A
import           Data.HVect hiding (pack)
import           Data.Monoid
import           Data.Text (Text, pack)
import           Data.Time.Clock (NominalDiffTime)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString,
                                    runSqlPool, getBy, insert, selectList, Entity(..), (==.),
                                    fromSqlKey)
import qualified Database.Persist.Sql as Sql
import           GHC.Generics (Generic)
import           GHC.Int (Int64)
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Web.Spock.Safe hiding (static)
import           Web.Users.Persistent
import           Web.Users.Types

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
sessionDur = 7 * 24 * 60 * 60 -- A week

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
  prehook initHook $
  do middleware $ staticPolicy (addBase "client/source.jsexe/")
     get root $ file "text/html" "client/source.jsexe/index.html"
     post "api/users" createUserRoute
     post "api/login" loginRoute
     prehook authHook $
       do get "api/counters" getCounters
          post "api/counters" createCounterAction
          get ("api/counters" <//> var) getCounter
          get ("api/counters" <//> var <//> "counts") getCounts

createUserRoute :: AppAction ctx sess ()
createUserRoute =
  do user <- jsonBody'
     res <- runUser (\b -> createUser (b :: Persistent) (user :: User String))
     case res of
       Right id -> do setStatus status200
                      text $ pack . show . fromSqlKey $ (id :: UserId Persistent)
       Left err -> do setStatus status500
                      text . pack . show $ err

loginRoute = 
  do login <- jsonBody'
     maybeSession <- runUser (\b -> authUser b (username login) (PasswordPlain . password $ login) sessionDur)
     case maybeSession of
       Just session -> text $ unSessionId session
       Nothing -> do
         setStatus status401
         text "Authentication failed"

getCounters :: ListContains n (UserId Persistent) xs => AppAction (HVect xs) sess ()
getCounters = do
  userId <- getUserId
  counters <- runDb $ selectList [CounterUserId ==. userId] []
  json (counters :: [Entity Counter])

getCounter :: ListContains n (UserId Persistent) xs => (Sql.Key Counter) -> AppAction (HVect xs) sess ()
getCounter counterId = do
  userId <- getUserId
  maybeCounter <- runDb $ Sql.get counterId
  case maybeCounter of
    Just counter -> do
      if (counterUserId counter) == userId
         then json (counter :: Counter)
         else setStatus status401 >> text ""
    Nothing -> do
      setStatus status404
      text "Counter not found"

getCounts :: ListContains n (UserId Persistent) xs => (Sql.Key Counter) -> AppAction (HVect xs) sess ()
getCounts counterId =
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
