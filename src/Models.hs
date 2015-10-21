{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Time
import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend(..), runMigration,
                                    runSqlPool)
import           Web.Users.Persistent
import           Web.Users.Types
import Database.Persist.Sql (runSqlConn)
import Database.Persist.TH         (share, mkPersist, sqlSettings, mpsPrefixFields,
                                    mkMigrate, persistLowerCase)
import Web.Spock.Shared

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Counter json
    name String
    userId (UserId Persistent)
    deriving Show Eq Generic
Count json
    time UTCTime default=now()
    countedForId CounterId
    delta Rational
    deriving Show Eq Generic
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb action = runQuery $ \conn -> runResourceT $ runNoLoggingT $ runSqlConn action conn
