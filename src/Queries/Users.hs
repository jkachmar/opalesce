{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Queries.Users where

import           Control.Arrow              (returnA)
import           Control.Lens
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Bool                  (bool)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    hiding (null)

import           Models.Users

---------------------------------------------------------------------------------

usersAll :: MonadIO m => Connection -> m [User]
usersAll conn = liftIO $ runQuery conn (queryTable userTable)

userQuery :: Query UserColumns
userQuery = queryTable userTable

findUserByIdQ :: UserIdColumn -> Query UserColumns
findUserByIdQ uId = proc () -> do
  user <- userQuery -< ()
  restrict -< unUserId (user ^. userId) .== unUserId uId
  returnA -< user

findUserById :: MonadIO m => Connection -> UserId -> m (Maybe User)
findUserById c uId =
  liftIO $ (^? _head) <$> runQuery c (findUserByIdQ (constant uId))

---------------------------------------------------------------------------------

findUserByNameQ :: (Column PGText) -> Query UserColumns
findUserByNameQ uName = proc () -> do
  user <- userQuery -< ()
  restrict -< user ^. userName .== uName
  returnA -< user

findUsersByName :: MonadIO m => Connection -> Text -> m (Maybe [User])
findUsersByName conn uName = do
  users <- liftIO $ runQuery conn (findUserByNameQ (constant uName))
  return $ bool Nothing (Just users) (null users)

---------------------------------------------------------------------------------

addUser :: MonadIO m => Connection -> Text -> Text -> m UserId
addUser conn uName uPass =
  let userInsert = User { _userId = UserId Nothing
                        , _userName = constant uName
                        , _userPass = constant uPass
                        }
  in liftIO $ head <$> runInsertReturning conn userTable userInsert (^. userId)
