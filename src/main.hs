{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-}

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

app :: Application
app = serve (Proxy :: Proxy FluexetteAPI) server

server = serveDirectory "../fluxette/js-build/install-root/bin/fluxette.jsexe"
    :<|> serveDirectory "webroot/css"
    :<|> serveDirectory "webroot/html"

type FluexetteAPI = "js" :> Raw
               :<|> "css" :> Raw
               :<|> "html" :> Raw

main :: IO ()
main = do
  let p = 8083
  putStrLn $ "Running fluxette at " ++ show p
  run p app
