{-# LANGUAGE TypeOperators #-}

module Api where

import           Servant

import           Api.Users
import           Config    (App (..), Config (..), convertApp)

--------------------------------------------------------------------------------

type AppApi = UserApi

appApi :: Proxy AppApi
appApi = Proxy

server :: ServerT AppApi App
server = userServer

--------------------------------------------------------------------------------

appToServer :: Config -> Server AppApi
appToServer cfg = enter (convertApp cfg) userServer

app :: Config -> Application
app cfg = serve (Proxy :: Proxy UserApi) (appToServer cfg)
