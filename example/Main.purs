module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.JSDate (LOCALE)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.HTTP (HTTP, ListenOptions, createServer, listen)
import StaticServe (staticHandler)



config :: ListenOptions
config =
  { hostname: "0.0.0.0"
  , port: 3000
  , backlog: Nothing
  }



main :: forall e. Eff (fs :: FS, http :: HTTP, locale :: LOCALE | e) Unit
main = do
  server <- createServer $ staticHandler { root: "./public", maxAge: 60 }
  listen server config $ pure unit
