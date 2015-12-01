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
import Text.Hamlet

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
    areq emailField FieldSettings{
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

formMassagista :: Form Massagista
formMassagista = renderDivsNoLabels $ Massagista <$>
    areq textField FieldSettings{
        fsId = Just("nome"),
        fsLabel = "Nome",
        fsTooltip = Nothing,
        fsName = Just ("nome"),
        fsAttrs = [("placeholder","Nome"),("class","form-control")]
    } Nothing <*>
    areq intField FieldSettings{
        fsId = Just("idade"),
        fsLabel = "Idade",
        fsTooltip = Nothing,
        fsName = Just ("idade"),
        fsAttrs = [("placeholder","Idade"),("class","form-control")]
    } Nothing <*>
    areq textField FieldSettings{
        fsId = Just("descricao"),
        fsLabel = "Descrição",
        fsTooltip = Nothing,
        fsName = Just ("descricao"),
        fsAttrs = [("placeholder","Habilidades"),("class","form-control")]
    } Nothing

formMassagem :: Form Massagem
formMassagem = renderDivsNoLabels $ Massagem <$>    
    areq(selectField usuarios) FieldSettings{
        fsId = Just("nome"),
        fsLabel = "Nome",
        fsTooltip = Nothing,
        fsName = Just ("nome"),
        fsAttrs = [("placeholder","Nome"),("class","form-control")]
    } Nothing <*>
    areq (selectField massagistas) FieldSettings{
        fsId = Just("categoria"),
        fsLabel = "Categoria",
        fsTooltip = Nothing,
        fsName = Just ("categoria"),
        fsAttrs = [("placeholder","Categoria"),("class","form-control")]
    } Nothing

massagistas = do
       entidades <- runDB $ selectList [] [Asc MassagistaNome] 
       optionsPairs $ fmap (\ent -> (massagistaNome $ entityVal ent, entityKey ent)) entidades

usuarios = do
       entidades <- runDB $ selectList [] [Asc UsuarioNome] 
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades

formLogin :: Form Usuario
formLogin = renderDivsNoLabels $ Usuario <$>
    lift (liftIO $ return "")<*>
    areq emailField FieldSettings{
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


-- -- -- -- -- FORMULÁRIO GENÉRICO
wForm :: Widget -> Widget
wForm hamlet = do
        hamlet
        toWidget $(luciusFile "lucius/imputs.lucius")

wHead :: Widget
wHead = do
    toWidgetHead [hamlet|
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link href=@{StaticR template_css_bootstrap_min_css} rel="stylesheet"/>
        <link href=@{StaticR template_css_freelancer_css} rel="stylesheet"/>
        <link href=@{StaticR template_font_awesome_4_1_0_css_font_awesome_min_css} rel="stylesheet" type="text/css"/>
    |]
    toWidget [julius|
        $("body").attr("id","page-top").attr("class","index");
        $("html").attr("lang","pt-br");
        $(".linha").hover(function(){
            $(this).css("cursor","pointer");
        });
        $(".linha").click(function(){
            window.location.href = $(this).data("id");
        });
        $("#email").focus();
    |]

wNav :: Bool -> Widget
wNav completa = do
    usr <- lookupSession "_NOME"
    $(whamletFile "hamlets/default/nav.hamlet")
    case completa of 
        False -> $(whamletFile "hamlets/home/script.hamlet")
        _ -> [whamlet| |]

getHomeR :: Handler Html
getHomeR = do
        lista <- runDB $ selectList [] [Asc MassagistaNome]
        defaultLayout $ do
            setTitle "Casa do Leléo"
            wHead
            wNav True
            $(whamletFile "hamlets/home/header.hamlet")
            $(whamletFile "hamlets/home/massagistas_grid.hamlet")
            $(whamletFile "hamlets/home/a_casa.hamlet")
            $(whamletFile "hamlets/home/footer.hamlet")
            $(whamletFile "hamlets/home/scroll_button.hamlet")
            $(whamletFile "hamlets/home/massagistas_modal.hamlet")
            $(whamletFile "hamlets/home/script.hamlet")

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype) <- generateFormPost formLogin
    msg <- getMessage
    let route = LoginR
    defaultLayout $ do
        setTitle "Login"
        wHead
        wNav False
        wForm $(whamletFile "hamlets/login/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioEmail ==. usuarioEmail usr, UsuarioSenha ==. usuarioSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_NOME" (usuarioNome usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| Usuário Inválido! |]
                    redirect LoginR
        _ -> redirect LoginR

-- ------------ USUARIOS

getUsuarioCadastrarR :: Handler Html
getUsuarioCadastrarR = do
    (widget,enctype) <- generateFormPost formUsuario
    msg <- getMessage
    let route = UsuarioCadastrarR
    defaultLayout $ do
        setTitle "Cadastro de Usuario"
        wHead
        wNav False
        wForm $(whamletFile "hamlets/usuarios/usuario_cadastrar.hamlet")

postUsuarioCadastrarR :: Handler Html
postUsuarioCadastrarR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| Usuario inserido com sucesso! |]
            redirect UsuarioCadastrarR
        _ -> redirect UsuarioCadastrarR

getUsuarioR :: UsuarioId -> Handler Html
getUsuarioR uid = do    
    usr <- runDB $ selectFirst [UsuarioId ==. uid] []
    case usr of
        Nothing -> redirect ListarUsuariosR
        Just usr -> do
            defaultLayout $ do
                setTitle "Usuario"
                wHead
                wNav False
                $(whamletFile "hamlets/usuarios/usuario.hamlet")

getUsuarioExcluirR :: UsuarioId -> Handler Html
getUsuarioExcluirR id = do
    runDB $ get404 id
    runDB $ delete $ id
    setMessage $ [shamlet| Registro excluído com sucesso! |]
    redirect ListarUsuariosR

getListarUsuariosR :: Handler Html
getListarUsuariosR = do
    listaUsuarios <- runDB $ selectList [] [Asc UsuarioNome]
    msg <- getMessage
    defaultLayout $ do
        setTitle "Lista de Massagistas"
        wHead
        wNav False
        $(whamletFile "hamlets/usuarios/usuarios_listar.hamlet")


-- ------------ MASSAGISTAS

getMassagistaCadastrarR :: Handler Html
getMassagistaCadastrarR = do
    (widget,enctype) <- generateFormPost formMassagista
    msg <- getMessage
    let route = MassagistaCadastrarR
    defaultLayout $ do
        setTitle "Cadastro de Massagista"
        wHead
        wNav False
        wForm $(whamletFile "hamlets/massagistas/massagista_cadastrar.hamlet")

postMassagistaCadastrarR :: Handler Html
postMassagistaCadastrarR = do
    ((result,_),_) <- runFormPost formMassagista
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| Massagista inserida com sucesso! |]
            -- redireciona para a massagista cadastrada
            -- redirect $ MassagistaR usrId 
            redirect MassagistaCadastrarR
        _ -> redirect MassagistaCadastrarR

getMassagistaR :: MassagistaId -> Handler Html
getMassagistaR uid = do    
    registro <- runDB $ selectFirst [MassagistaId ==. uid] []
    case registro of
        Nothing -> redirect ListarMassagistasR
        Just registro -> do
            defaultLayout $ do
                setTitle "Massagista"
                wHead
                wNav False
                $(whamletFile "hamlets/massagistas/massagista.hamlet")

getMassagistaExcluirR :: MassagistaId -> Handler Html
getMassagistaExcluirR id = do
    runDB $ get404 id
    runDB $ delete $ id
    setMessage $ [shamlet| Registro excluído com sucesso! |]
    redirect ListarMassagistasR

getListarMassagistasR :: Handler Html
getListarMassagistasR = do
    lista <- runDB $ selectList [] [Asc MassagistaNome]
    msg <- getMessage
    defaultLayout $ do
        setTitle "Lista de Massagistas"
        wHead
        wNav False
        $(whamletFile "hamlets/massagistas/massagistas_listar.hamlet")

-- ------------ AGENDA
{--
getAgendarR :: UsuarioId -> Handler Html
getAgendarR uid = do
    registro <- runDB $ selectFirst [UsuarioId ==. uid] []
    case registro of
        Nothing -> redirect HomeR
        Just registro -> do
            (w,e) <- generateFormPost $ formAgenda $ entityKey registro
            defaultLayout $ do
                setTitle "Cadastro de Massagista"
                wHead
                wNav False
                (wFormAgenda (AgendarR uid) w e)
--}

-- ------------ Massagens

getMassagemCadastrarR :: Handler Html
getMassagemCadastrarR = do
    (widget,enctype) <- generateFormPost formMassagem
    msg <- getMessage
    let route = MassagemCadastrarR
    defaultLayout $ do
        setTitle "Cadastro de Massagem"
        wHead
        wNav False
        wForm $(whamletFile "hamlets/massagens/massagem_cadastrar.hamlet")

postMassagemCadastrarR :: Handler Html
postMassagemCadastrarR = do
    ((result,_),_) <- runFormPost formMassagem
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| Massagem inserida com sucesso! |]            
            redirect MassagemCadastrarR
        _ -> redirect MassagemCadastrarR

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_NOME"
    defaultLayout $ do
        setTitle "Lista de Massagistas"
        wHead
        wNav False
        $(whamletFile "hamlets/logout/logout.hamlet")
        toWidgetHead [julius|
            setTimeout( "window.location = '@{HomeR}'", 5000);
        |]

getRelMassagensR :: Handler Html
getRelMassagensR = do
    massagens <- runDB $ (rawSql "SELECT ??, ??, ?? \
                \ FROM massagem \
                \ INNER JOIN usuario       ON massagem.uid = usuario.id \
                \ INNER JOIN massagista    ON massagem.mid = massagista.id \
                \"[])::Handler [(Entity Massagem, Entity Usuario, Entity Massagista)]
    msg <- getMessage
    defaultLayout $ do
        setTitle "Lista de Massagistas"
        wHead
        wNav False
        $(whamletFile "hamlets/relatorios/massagens.hamlet")

getAdminR :: Handler Html
getAdminR = do defaultLayout [whamlet| Bem vindo admin |]

connStr = "dbname=dffsbj3768pl57 host=ec2-204-236-226-63.compute-1.amazonaws.com user=ejechbrbnfdngp password=hBK8ie6GEvDQmxohNAWgj1w2Ug port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warp 8080 (Sitio pool s)
