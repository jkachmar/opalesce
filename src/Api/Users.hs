{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Users where

import           Data.Text
import           Servant

import           Config        (App (..), getConnection)
import           Models.Users
import           Queries.Users

--------------------------------------------------------------------------------

type UserApi = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "username" Text :> Get '[JSON] User

userApi :: Proxy UserApi
userApi = Proxy

userServer :: ServerT UserApi App
userServer = getUsers :<|> singleUserByName

--------------------------------------------------------------------------------

getUsers :: App [User]
getUsers = usersAll =<< getConnection

--------------------------------------------------------------------------------

userExists :: Maybe User -> App User
userExists = maybe (throwError err404) (return)

singleUserById :: UserId -> App User
singleUserById uid = do
  user <- (flip findUserById $ uid) =<< getConnection
  userExists user

singleUserByName :: Text -> App User
singleUserByName username = do
  user <- (flip findUsersByName $ username) =<< getConnection
  userExists user

--------------------------------------------------------------------------------

createUser :: Text -> Text -> App UserId
createUser username password = do
  conn <- getConnection
  uid <- addUser conn username password
  case uid of
    Left _ -> throwError $ err400 { errBody = "Username already exists!" }
    Right uid' -> return uid'
