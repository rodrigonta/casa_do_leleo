{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Routers where

import Yesod
import Yesod.Static

pRoutes = [parseRoutes|
   / HomeR GET

   /usuarios/cadastrar UsuarioCadastrarR GET POST
   /usuarios/usuario/#UsuarioId UsuarioR GET
   /usuarios/excluir/#UsuarioId UsuarioExcluirR GET
   /usuarios/listar ListarUsuariosR GET

   /massagistas/cadastrar MassagistaCadastrarR GET POST
   /massagistas/massagista/#MassagistaId MassagistaR GET
   /massagistas/excluir/#MassagistaId MassagistaExcluirR GET
   /massagistas/listar ListarMassagistasR GET

   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR GET POST
   /logout LogoutR GET
   /admin AdminR GET
|]
