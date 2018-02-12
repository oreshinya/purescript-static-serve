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
import Data.JSDate (JSDate, LOCALE, getTime, parse, toUTCString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.StrMap (lookup)
import Data.String (Pattern(..), indexOf, lastIndexOf)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Node.FS (FS)
import Node.FS.Async (stat)
import Node.FS.Stats (Stats(..))
import Node.FS.Stream (createReadStream)
import Node.HTTP (HTTP, Request, Response, requestHeaders, requestMethod, requestURL, responseAsStream, setHeader, setStatusCode)
import Node.Path (resolve)
import Node.Stream (end, onError, pipe)
import Node.URL as URL
import StaticServe.ContentType (contentTypeFromPath)



type Settings =
  { root :: String
  , maxAge :: Int
  , historyAPIFallback :: Boolean
  }



foreign import code :: Error -> String



isHeadOrGet :: Request -> Boolean
isHeadOrGet req =
  isHead req || requestMethod req == "GET"



isHead :: Request -> Boolean
isHead req = requestMethod req == "HEAD"



requestHeader :: String -> Request -> Maybe String
requestHeader key req = lookup key $ requestHeaders req



fresh :: forall e. Request -> JSDate -> Eff (locale :: LOCALE | e) Boolean
fresh req lastModified =
  case requestHeader "if-modified-since" req of
    Nothing -> pure false
    Just timeStr -> do
      ifModifiedSince <- parse timeStr
      if isNoCache
        then pure false
        else pure $ getTime lastModified <= getTime ifModifiedSince
  where
    isNoCache =
      maybe false includeNoCache $ requestHeader "cache-control" req
    includeNoCache =
      test $ unsafeRegex "(?:^|,)\\s*?no-cache\\s*?(?:,|$)" noFlags



getPath :: Settings -> Request -> String
getPath settings req = if settings.historyAPIFallback then fallbackPath else path
  where
    path =
      fromMaybe "/"
        $ toMaybe
        $ _.pathname
        $ URL.parse
        $ requestURL req

    fallbackPath =
      case requestHeader "accept" req of
        Nothing -> path
        Just accept ->
          if fallback accept path
            then "/index.html"
            else path



preferJSON :: String -> Boolean
preferJSON accept =
  case indexOf (Pattern "application/json") accept of
    Just 0 -> true
    _ -> false



acceptHTML :: String -> Boolean
acceptHTML accept =
  case indexOf (Pattern "text/html") accept, indexOf (Pattern "*/*") accept of
    Nothing, Nothing -> false
    _, _ -> true



directFileRequest :: String -> Boolean
directFileRequest path =
  case lastIndexOf (Pattern ".") path, lastIndexOf (Pattern "/") path of
    Just i1, Just i2 | i1 > i2 -> true
    Just i1, Nothing -> true
    _, _ -> false



fallback :: String -> String -> Boolean
fallback accept path =
  (not $ preferJSON accept) && acceptHTML accept && (not $ directFileRequest path)



staticHandler
  :: forall e
   . Settings
  -> (Request -> Response -> Eff (http :: HTTP, fs :: FS, locale :: LOCALE | e) Unit)
staticHandler settings req res =
  if isHeadOrGet req
    then stat fullPath handleStat
    else handleInvalidMethod
  where
    fullPath = resolve [ settings.root ] $ "." <> getPath settings req

    handleInvalidMethod = do
      setStatusCode res 404
      end (responseAsStream res) $ pure unit

    handleStat (Left err) = do
      setStatusCode res
        if elem (code err) [ "ENOENT", "ENAMETOOLONG", "ENOTDIR" ]
          then 404
          else 500
      end (responseAsStream res) $ pure unit

    handleStat (Right (Stats stats)) = do
      setHeader res "Content-Type" $ contentTypeFromPath fullPath
      setHeader res "Last-Modified" $ toUTCString stats.mtime
      setHeader res "Cache-Control" $ "max-age=" <> show settings.maxAge
      if isHead req
        then handleHead
        else handleGet stats

    handleHead = do
      setStatusCode res 200
      end (responseAsStream res) $ pure unit

    handleGet stats = do
      isFresh <- parse (toUTCString stats.mtime) >>= fresh req
      if isFresh
        then do
          setStatusCode res 304
          end (responseAsStream res) $ pure unit
        else do
          setStatusCode res 200
          readable <- createReadStream fullPath
          onError readable \err -> handleStat $ Left err
          void $ pipe readable $ responseAsStream res
