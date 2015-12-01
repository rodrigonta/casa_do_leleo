{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}

module Foundation where
import Routers
import Yesod
import Yesod.Static
import Data.Text
import Control.Applicative
import Data.Text (Text)
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)

import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
   nome Text
   email Text
   senha Text
   deriving Show
Massagista
   nome Text
   idade Int
   habilidade Text
   deriving Show
Massagem
    uid UsuarioId
    mid MassagistaId
    deriving Show
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
  authRoute _ = Just $ LoginR
  isAuthorized AdminR _ = isAdmin
  isAuthorized HomeR _ = return Authorized
  isAuthorized LogoutR _ = return Authorized
  isAuthorized _ _ = isUser


isUser = do
    mu <- lookupSession "_NOME"
    return $ case mu of
      Nothing -> AuthenticationRequired
      Just _ -> Authorized


isAdmin = do
  mu <- lookupSession "_NOME"
  return $ case mu of
      Nothing -> AuthenticationRequired
      Just "admin" -> Authorized
      Just _ -> Unauthorized "Soh o admin acessa"


type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
