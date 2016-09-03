

import Network.Wai
import Network.Wai.Handler.Warp

import Model

import Control.Concurrent.MVar
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad

import qualified Text.Blaze.Html5 as BH

import Data.Proxy
import Routes

import Servant

app :: MVar Game -> Application
app v = serve (Proxy :: Proxy FluxetteAPI) (server v)

main :: IO ()
main = do
  let p = 8083
  g <- initGame
  v <- newMVar g
  putStrLn $ "Running fluxette at " ++ show p
  run p (app v)
