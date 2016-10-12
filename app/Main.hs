module Main where

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Api                                  (app)
import           Config                               (Config (..),
                                                       Environment (..),
                                                       makePool)

main :: IO ()
main = do
  let port = 8081
  pool <- makePool Test
  let cfg = Config { getPool = pool, getEnv = Test }
  run port $ logStdoutDev $ app cfg
