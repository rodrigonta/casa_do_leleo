{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routers where

import Yesod
import Yesod.Static

pRoutes = [parseRoutes|
   / HomeR GET

   /usuarios/insert UsuarioCadastrarR GET POST
   /usuarios/select/#UsuarioId UsuarioR GET
   /usuarios/delete/#UsuarioId UsuarioExcluirR GET
   /usuarios/all ListarUsuariosR GET

   /massagistas/insert MassagistaCadastrarR GET POST
   /massagistas/select/#MassagistaId MassagistaR GET
   /massagistas/delete/#MassagistaId MassagistaExcluirR GET
   /massagistas/all ListarMassagistasR GET

   /static StaticR Static getStatic   
   /login LoginR GET POST
   /logout LogoutR GET
   /admin AdminR GET
|]
