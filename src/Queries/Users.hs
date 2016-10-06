{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Queries.Users where

import           Control.Arrow              (returnA)
import           Control.Lens
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    hiding (null)

import           Models.Users

---------------------------------------------------------------------------------

usersAll :: MonadIO m => Connection -> m [User]
usersAll conn = liftIO $ runQuery conn (queryTable userTable)

userQuery :: Query UserColumns
userQuery = queryTable userTable

---------------------------------------------------------------------------------

restrictByUserId :: UserIdColumn -> QueryArr UserColumns ()
restrictByUserId uId = proc user -> do
  restrict -< unUserId (user ^. userId) .== unUserId uId

-- | Construct a query matching against the user's id field
findUserByIdQ :: UserIdColumn -> Query UserColumns
findUserByIdQ uId = proc () -> do
  user <- userQuery -< ()
  restrictByUserId uId -< user
  returnA -< user

findUserById :: MonadIO m => Connection -> UserId -> m (Maybe User)
findUserById c uId =
  liftIO $ (^? _head) <$> runQuery c (findUserByIdQ (constant uId))

---------------------------------------------------------------------------------

restrictByUserName :: Column PGText -> QueryArr UserColumns ()
restrictByUserName uName = proc user -> do
  restrict -< (user ^. userName) .== uName

-- | Construct a query matching against the username field
findUserByNameQ :: Column PGText -> Query UserColumns
findUserByNameQ uName = proc () -> do
  user <- userQuery -< ()
  restrictByUserName uName -< user
  returnA -< user

-- | Run the username query
findUsersByName :: MonadIO m => Connection -> Text -> m (Maybe User)
findUsersByName conn uName = do
  users <- liftIO $ (^? _head) <$> (runQuery conn (findUserByNameQ (constant uName)))
  return users

---------------------------------------------------------------------------------

-- | This query is the composition of the two restrictions above
findUsersByNameAndIdQ :: UserIdColumn -> Column PGText -> Query UserColumns
findUsersByNameAndIdQ uId uName = proc () -> do
  user <- userQuery -< ()
  restrictByUserId uId -< user
  restrictByUserName uName -< user
  returnA -< user

---------------------------------------------------------------------------------

-- | Try to add a user, returning Left (error message) if the name exists, or
-- Right (added user id) if there is no conflict
addUser :: MonadIO m => Connection -> Text -> Text -> m (Either Text UserId)
addUser conn uName uPass =
  let userInsert = User { _userId   = UserId Nothing
                        , _userName = constant uName
                        , _userPass = constant uPass
                        }
      insertUser = runInsertReturning conn userTable userInsert (^. userId)
  in do
    exists <- findUsersByName conn uName
    case exists of
      Nothing -> return . Left $ "Username already exists!"
      Just _  -> return . Right =<< (liftIO $ head <$> insertUser)
