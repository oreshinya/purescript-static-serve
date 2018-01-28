module StaticServe
  ( Settings
  , isHeadOrGet
  , staticHandler
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.JSDate (toUTCString)
import Node.FS (FS)
import Node.FS.Async (stat)
import Node.FS.Stats (Stats(..))
import Node.FS.Stream (createReadStream)
import Node.HTTP (HTTP, Request, Response, requestMethod, requestURL, responseAsStream, setHeader, setStatusCode)
import Node.Path (resolve)
import Node.Stream (end, onError, pipe)
import StaticServe.ContentType (contentTypeFromPath)



type Settings =
  { root :: String
  , maxAge :: Int
  }



foreign import code :: Error -> String



isHeadOrGet :: Request -> Boolean
isHeadOrGet req =
  isHead req || requestMethod req == "GET"



isHead :: Request -> Boolean
isHead req = requestMethod req == "HEAD"



staticHandler :: forall e. Settings -> (Request -> Response -> Eff (http :: HTTP, fs :: FS | e) Unit)
staticHandler { root, maxAge } req res =
  if isHeadOrGet req
    then stat fullPath handleStat
    else handleInvalidMethod
  where
    path = requestURL req

    fullPath = resolve [ root ] $ "." <>
      if path == "/"
        then "/index.html"
        else path

    handleInvalidMethod = do
      setHeader res "Connection" "close"
      setHeader res "Content-Type" "text/html; charset=utf-8"
      setStatusCode res 404
      end (responseAsStream res) $ pure unit

    handleStat (Left err) = do
      setHeader res "Connection" "close"
      setHeader res "Content-Type" "text/html; charset=utf-8"
      setStatusCode res
        if elem (code err) [ "ENOENT", "ENAMETOOLONG", "ENOTDIR" ]
          then 404
          else 500
      end (responseAsStream res) $ pure unit

    handleStat (Right (Stats stats)) = do
      setHeader res "Connection" "close"
      setHeader res "Content-Type" $ contentTypeFromPath fullPath
      setHeader res "Content-Length" $ show stats.size
      setHeader res "Last-Modified" $ toUTCString stats.mtime
      setHeader res "Cache-Control" $ "max-age=" <> show maxAge
      setStatusCode res 200
      if isHead req
        then
          end (responseAsStream res) $ pure unit
        else do
          readable <- createReadStream fullPath
          onError readable \err -> handleStat $ Left err
          void $ pipe readable $ responseAsStream res
