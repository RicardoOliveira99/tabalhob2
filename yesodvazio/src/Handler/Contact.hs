{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Contact where

import Import
import Text.Lucius
import Text.Julius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

formContactR :: Maybe Contact -> Form Contact
formContactR mc = renderDivs $ Contact 
    <$> areq textField (FieldSettings ""
                                   (Just "Nome")
                                   (Just "nome")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap contactNome mc)
    <*> areq textField (FieldSettings ""
                                   (Just "E-mail")
                                   (Just "email")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap contactEmail mc)
    <*> areq intField (FieldSettings ""
                                    (Just "Celular")
                                    (Just "cel")
                                    Nothing
                                    [("id","content"),
                                    ("class","content-form")]
                        ) (fmap contactCelular mc)
    <*> areq textareaField (FieldSettings ""
                                   (Just "Comentario")
                                   (Just "text")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap contactMensagem mc)



getContactR :: Handler Html
getContactR = do   
    (widget,_) <- generateFormPost (formContactR Nothing)
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/formContato.lucius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/contatosub.hamlet")

postContactR :: Handler Html
postContactR = do 
    ((result,_),_) <- runFormPost (formContactR Nothing)
    case result of 
        FormSuccess contact -> do
            runDB $ insert contact
            setMessage [shamlet|
                <div>
                    MENSAGEM ENVIADA COM SUCESSO!
            |]
            redirect ContactR
        _ -> redirect HomeR


getListComentsR :: Handler Html
getListComentsR = do
     comentarios <- runDB $ selectList [] [Asc ContactNome]
     defaultLayout $ do
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarComentarios.hamlet")
        

getListaComentsR :: ContactId -> Handler Html
getListaComentsR pid = do
    contact <- runDB $ get404 pid 
    defaultLayout [whamlet|
        <h2>
            Nome: #{contactNome contact}
        <h2>
            E-mail: #{contactEmail contact}
        <h2>
            Celular: #{contactCelular contact}
        <h2>
            Comentario: #{contactMensagem contact}
    |]


postApagarComentR :: ContactId -> Handler Html
postApagarComentR pid = do
    runDB $ delete pid
    redirect ListComentsR


getEditarComentR :: ContactId -> Handler Html
getEditarComentR pid = do
    contact <- runDB $ get404 pid
    (widget,_) <- generateFormPost (formContactR (Just contact))
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/formContato.lucius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/contatosubEditar.hamlet")
        

postEditarComentR :: ContactId -> Handler Html
postEditarComentR pid = do
    _ <- runDB $ get404 pid
    ((result,_),_) <-runFormPost (formContactR Nothing)
    case result of
        FormSuccess novoContact -> do
            runDB $ replace pid novoContact
            redirect ListComentsR
        _ -> redirect HomeR







