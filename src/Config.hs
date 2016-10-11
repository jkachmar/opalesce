{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Config where

import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks,
                                             runReaderT)
import qualified Data.ByteString.Char8      as BS
import           Data.Pool                  (Pool, createPool, withResource)
import           Database.PostgreSQL.Simple (Connection, close,
                                             connectPostgreSQL)
import           Servant                    ((:~>) (..), ServantErr (..))

--------------------------------------------------------------------------------

data Environment = Test | Production

data Config = Config
  { getPool :: Pool Connection
  , getEnv  :: Environment
  }

--------------------------------------------------------------------------------

newtype App a = App
  { runApp :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config
             , MonadError ServantErr, MonadIO )

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

--------------------------------------------------------------------------------

getConnection :: (MonadReader Config m, MonadIO m) => m Connection
getConnection = do
  pool <- asks getPool
  liftIO $ withResource pool return

makePool :: Environment -> IO (Pool Connection)
makePool _ = createPool (connectPostgreSQL connStr) close 1 10 1

connStr :: BS.ByteString
connStr = "host=localhost dbname=opal user=test password=test port=5432"
