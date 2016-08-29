{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-}

import Servant
import Servant.HTML.Blaze

import Network.Wai
import Network.Wai.Handler.Warp

import Model

import Control.Concurrent.MVar
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad

import qualified Text.Blaze.Html5 as BH

app :: MVar Game -> Application
app v = serve (Proxy :: Proxy FluexetteAPI) (server v)

server v = serveDirectory "../fluxette/js-build/install-root/bin/fluxette.jsexe"
    :<|> serveDirectory "webroot/css"
    :<|> serveDirectory "webroot/html"
    :<|> currentGame v

type FluexetteAPI = "js" :> Raw
               :<|> "css" :> Raw
               :<|> "html" :> Raw
               :<|> "currentGame" :> Get '[JSON] Game

newtype MyInt = MyInt Int

instance BH.ToMarkup MyInt where
  toMarkup (MyInt x) = BH.toHtml $ "The current value is: " ++ (show x)

currentGame :: MVar Game -> EitherT ServantErr IO Game
currentGame v = do
  x <- liftIO $ readMVar v
  return x

counter :: MVar Int -> EitherT ServantErr IO MyInt
counter v = do
  -- e <- liftIO $ isEmptyMVar v
  -- when e (liftIO $ putMVar v 38)
  x <- liftIO $ takeMVar v
  liftIO $ putMVar v (x + 1)
  return (MyInt $ x + 1)

main :: IO ()
main = do
  let p = 8083
  g <- initGame
  v <- newMVar g
  putStrLn $ "Running fluxette at " ++ show p
  run p (app v)
