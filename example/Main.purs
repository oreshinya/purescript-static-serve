module Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Node.HTTP (ListenOptions, createServer, listen)
import StaticServe (staticHandler)



config :: ListenOptions
config =
  { hostname: "0.0.0.0"
  , port: 3000
  , backlog: Nothing
  }



main :: Effect Unit
main = do
  server <- createServer $ staticHandler { root: "./public", maxAge: 60, historyAPIFallback: true }
  listen server config $ pure unit
