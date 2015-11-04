{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Applicative
import Yesod


data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Fornecedor
   nome Text
   deriving Show

Peca
   nome Text sqltype=varchar(20)
   descricao Text
   estoque Int

Ordem
   fornId FornecedorId
   pecaId PecaId
   qtde Int
   data UTCTime default=now()
   processado Bool
   UniquePersonStore fornId pecaId
|]

mkYesod "Pagina" [parseRoutes|
  /peca PecaR GET POST
  /forncedor FornR GET POST
  /listpeca ListarPecaR GET
  /listforn ListarFornR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formPeca :: Form Peca
formPeca = renderDivs $ Peca <$>
             areq textField "Nome" Nothing <*>
             areq textField "Desc" Nothing <*>
             areq intField "Qtde Estoque" Nothing

formForn :: Form Fornecedor
formForn = renderDivs $ Fornecedor <$>
             areq textField "Nome" Nothing 

widgetForm :: Route Pagina -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

getPecaR :: Handler Html
getPecaR = do
           (widget, enctype) <- generateFormPost formPeca
           defaultLayout $ widgetForm PecaR enctype widget "Pecas"

postPecaR :: Handler Html
postPecaR = do
            ((result,_),_) <- runFormPost formPeca
            case result of
                FormSuccess peca -> (runDB $ insert peca) >> defaultLayout [whamlet|<h1> Peca inserida|]
                _ -> redirect PecaR


getFornR :: Handler Html
getFornR = do
           (widget, enctype) <- generateFormPost formForn
           defaultLayout $ widgetForm FornR enctype widget "Fornecedores"

postFornR :: Handler Html
postFornR = do
            ((result,_),_) <- runFormPost formForn
            case result of
                FormSuccess forn -> (runDB $ insert forn) >> defaultLayout [whamlet|<h1> Forn inserido|]
                _ -> redirect FornR

getListarPecaR :: Handler Html
getListarPecaR = do
                 pecas <- runDB $ selectList [] [Asc PecaNome]
                 defaultLayout [whamlet|
                      <h1> Lista de Pecas
                      $forall Entity pid pent <- pecas
                          <h2> #{pecaNome pent}
                 |]

getListarFornR :: Handler Html
getListarFornR = do
                 forns <- runDB $ selectList [] [Asc FornecedorNome]
                 defaultLayout [whamlet|
                      <h1> Lista de Fornecedores
                      $forall Entity fid fent <- forns
                          <h2> #{fornecedorNome fent}
                 |]

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Pagina pool)