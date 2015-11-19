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
--import qualified Database.Esqueleto as E
--import Database.Esqueleto ((^.))
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
   UniqueFornPeca fornId pecaId
|]

mkYesod "Pagina" [parseRoutes|
  /peca PecaR GET POST
  /forncedor FornR GET POST
  /listpeca ListarPecaR GET
  /listforn ListarFornR GET
  /ordem OrdemR GET POST
  / ListarOrdemR GET
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

formOrdem :: Form Ordem
formOrdem = renderDivs $ Ordem <$>
             areq (selectField forns) "Peca" Nothing <*>
             areq (selectField pecas) "Forn" Nothing <*>
             areq intField "Qtde" Nothing <*>
             lift (liftIO getCurrentTime) <*>
             lift (liftIO $ return False)

pecas = do
       entidades <- runDB $ selectList [] [Asc PecaNome] 
       optionsPairs $ fmap (\ent -> (pecaNome $ entityVal ent, entityKey ent)) entidades

forns = do
       entidades <- runDB $ selectList [] [Asc FornecedorNome] 
       optionsPairs $ fmap (\ent -> (fornecedorNome $ entityVal ent, entityKey ent)) entidades

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

getOrdemR :: Handler Html
getOrdemR = do
           (widget, enctype) <- generateFormPost formOrdem
           defaultLayout $ widgetForm OrdemR enctype widget "Ordens"

postOrdemR :: Handler Html
postOrdemR = do
            ((result,_),_) <- runFormPost formOrdem
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Ordem inserida|]
                _ -> redirect OrdemR

getListarOrdemR :: Handler Html
getListarOrdemR = do
                 ordens <- runDB $ (rawSql "SELECT ??, ?? \
                                   \FROM ordem INNER JOIN peca \
                                   \ON ordem.peca_id=peca.id" [])::Handler [(Entity Ordem, Entity Peca)]
                 defaultLayout [whamlet|
                      <h1> Lista de Ordens
                      $forall (Entity oq ordem, Entity _ np) <- ordens
                          <p> Ordem do dia #{show $ utctDay $ ordemData ordem} #{fromSqlKey oq}: #{pecaNome np}
                 |]

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Pagina pool)