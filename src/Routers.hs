{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routers where

import Yesod
import Yesod.Static

pRoutes = [parseRoutes|
   / HomeR GET
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR POST
   /restrita RestritaR GET
   /bye ByeR GET
   /admin AdminR GET
|]
