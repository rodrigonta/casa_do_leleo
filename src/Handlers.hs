{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Routers
import Yesod
import Yesod.Static
import Yesod.Form.Bootstrap3
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Monoid
import Text.Lucius
import Text.Julius

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formUsuario :: Form Usuario
formUsuario = renderDivsNoLabels $ Usuario <$>
    areq textField FieldSettings{
        fsId = Just("nome"),
        fsLabel = "Nome",
        fsTooltip = Nothing,
        fsName = Just ("nome"),
        fsAttrs = [("placeholder","Nome"),("class","form-control")]
    } Nothing <*>
    areq textField FieldSettings{
        fsId = Just("email"),
        fsLabel = "Email",
        fsTooltip = Nothing,
        fsName = Just ("email"),
        fsAttrs = [("placeholder","Email"),("class","form-control")]
    } Nothing <*>
    areq passwordField FieldSettings{
        fsId = Just("senha"),
        fsLabel = "Senha",
        fsTooltip = Nothing,
        fsName = Just ("senha"),
        fsAttrs = [("placeholder","Senha"),("class","form-control")]
    } Nothing

formLogin :: Form Usuario
formLogin = renderDivsNoLabels $ Usuario <$>
    areq hiddenField FieldSettings{
        fsId = Just("nome"),
        fsLabel = "Nome",
        fsTooltip = Nothing,
        fsName = Just ("nome"),
        fsAttrs = [("placeholder","Nome"),("class","form-control")]
    } Nothing <*>
    areq textField FieldSettings{
        fsId = Just("email"),
        fsLabel = "Email",
        fsTooltip = Nothing,
        fsName = Just ("email"),
        fsAttrs = [("placeholder","Email"),("class","form-control")]
    } Nothing <*>
    areq passwordField FieldSettings{
        fsId = Just("senha"),
        fsLabel = "Senha",
        fsTooltip = Nothing,
        fsName = Just ("senha"),
        fsAttrs = [("placeholder","Senha"),("class","form-control")]
    } Nothing

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "codigos/form.hamlet")
     toWidget $(luciusFile "codigos/teste.lucius")

wFormUsuario ::  Route Sitio -> Enctype -> Widget -> Widget
wFormUsuario route enctype widget = do
        msg <- getMessage
        $(whamletFile "hamlets/cadastrar_usuario.hamlet")
        toWidget $(luciusFile "lucius/imputs.lucius")

wFormLogin ::  Route Sitio -> Enctype -> Widget -> Widget
wFormLogin route enctype widget = do
        msg <- getMessage
        $(whamletFile "hamlets/login.hamlet")
        toWidget $(luciusFile "lucius/imputs.lucius")

-- Como deixar arquivo.hamlet no Head sem criar widget
-- exemplo toWidgetHead $(whamletFile "hamlets/nav.hamlet")

wHead :: Widget
wHead = toWidgetHead [hamlet|
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href=@{StaticR css_bootstrap_min_css} rel="stylesheet"/>
    <link href=@{StaticR css_freelancer_css} rel="stylesheet"/>
    <link href=@{StaticR font_awesome_4_1_0_css_font_awesome_min_css} rel="stylesheet" type="text/css"/>
|]


wHome :: Widget
wHome = do
    wHead
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/header.hamlet")
    $(whamletFile "hamlets/session_portifolio.hamlet")
    $(whamletFile "hamlets/session_about.hamlet")
    $(whamletFile "hamlets/session_contact.hamlet")
    $(whamletFile "hamlets/footer.hamlet")
    $(whamletFile "hamlets/scroll_button.hamlet")
    $(whamletFile "hamlets/portifolio.hamlet")
    $(whamletFile "hamlets/script.hamlet")
    toWidget $(juliusFile "julius/home.julius")

wRestrita :: Widget
wRestrita = do
    wHead
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/header.hamlet")

getHomeR :: Handler Html
getHomeR = do
           (wF1,eF1) <- generateFormPost formUsuario
           (wF2,eF2) <- generateFormPost formLogin
           defaultLayout $ do
               setTitle "Casa do Leléo"
               wHome
               (wFormUsuario UsuarioR eF1 wF1)
               (wFormLogin LoginR eF2 wF2)


{-- Não utilizado devido o form de login está na ModalLogin
getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ widgetForm LoginR enc wid "" "Log in"
--}

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioEmail ==. usuarioEmail usr, UsuarioSenha ==. usuarioSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_EMAIL" (usuarioEmail usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect HomeR
        _ -> redirect HomeR


getRestritaR :: Handler Html
getRestritaR = defaultLayout $ do
        wRestrita


getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar"

getImgR :: Handler Html
getImgR = defaultLayout [whamlet| <img src=@{StaticR codigos_empolgou_jpg}> |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $(whamletFile "codigos/list.hamlet")

getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| BYE! |]

getAdminR :: Handler Html
getAdminR = do defaultLayout [whamlet| Bem vindo admin |]

connStr = "dbname=dffsbj3768pl57 host=ec2-204-236-226-63.compute-1.amazonaws.com user=ejechbrbnfdngp password=hBK8ie6GEvDQmxohNAWgj1w2Ug port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)
