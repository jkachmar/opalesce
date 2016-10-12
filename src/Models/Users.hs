{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Models.Users where

import           Control.Lens               (makeLenses)
import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH
import           Data.Text
import           Opaleye

---------------------------------------------------------------------------------

data UserId' a = UserId { unUserId :: a } deriving Show
$(makeAdaptorAndInstance  "pUserId" ''UserId')

type UserIdColumn = UserId' (Column PGInt8)
type UserIdColumnMaybe = UserId' (Maybe (Column PGInt8))

type UserId = UserId' Int64

---------------------------------------------------------------------------------

data User' a b c = User
  { _userId   :: a
  , _userName :: b
  , _userPass :: c
  }
makeLenses ''User'
$(makeAdaptorAndInstance  "pUser" ''User')

type UserColumns = User' UserIdColumn (Column PGText) (Column PGText)
type UserInsertColumns = User' UserIdColumnMaybe (Column PGText) (Column PGText)
type User = User' UserId Text Text

instance ToJSON UserId where
  toJSON uid = object [ "id" .= unUserId uid ]

instance ToJSON User where
  toJSON User{..} = object [ "id"       .= unUserId _userId
                           , "username" .= _userName
                           , "password" .= _userPass
                           ]

---------------------------------------------------------------------------------

userTable :: Table UserInsertColumns UserColumns
userTable = Table "users" $ pUser User
  { _userId = pUserId . UserId $ optional "id"
  , _userName = required "username"
  , _userPass = required "password"
  }

---------------------------------------------------------------------------------
