{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.PedirNovo where

import Import
import Text.Lucius
import Text.Julius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

formPedirNovoR :: Maybe PedirNovo -> Form PedirNovo
formPedirNovoR pn = renderDivs $ PedirNovo 
    <$> areq textField (FieldSettings ""
                                   (Just "Tipo de papel")
                                   (Just "papel")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap pedirNovoPapel pn)
    <*> areq textareaField (FieldSettings ""
                                   (Just "Descrição")
                                   (Just "text")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap pedirNovoDescri pn)
    <*> areq textField (FieldSettings ""
                                   (Just "Tipo de coloração")
                                   (Just "cor")
                                   Nothing
                                   [("id","content"),
                                   ("class","content-form")]
                      ) (fmap pedirNovoPintura pn)



getPedirNovoR :: Handler Html
getPedirNovoR = do   
    (widget,_) <- generateFormPost (formPedirNovoR Nothing)
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/formContato.lucius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/pedirOriginal.hamlet")

postPedirNovoR :: Handler Html
postPedirNovoR = do 
    ((result,_),_) <- runFormPost (formPedirNovoR Nothing)
    case result of 
        FormSuccess pedirNovo -> do
            runDB $ insert pedirNovo
            setMessage [shamlet|
                <div>
                    Pedido novo registrado!
            |]
            redirect PedirNovoR
        _ -> redirect HomeR

getListPedidosOriginaisR :: Handler Html
getListPedidosOriginaisR = do
     pedidos <- runDB $ selectList [] [Asc PedirNovoPapel]
     defaultLayout $ do
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/listarPedidosNovos.hamlet")


getListapedidosOriginaisR :: PedirNovoId -> Handler Html
getListapedidosOriginaisR cid = do
    pedirNovo <- runDB $ get404 cid 
    defaultLayout [whamlet|
        <h2>
            Papel: #{pedirNovoPapel pedirNovo}
        <h2>
            Descrição: #{pedirNovoDescri pedirNovo}
        <h2>
            Tipo de coloração: #{pedirNovoPintura pedirNovo}
    |]


postApagarOriginalR :: PedirNovoId -> Handler Html
postApagarOriginalR cid = do
    runDB $ delete cid
    redirect ListPedidosOriginaisR


getEditarOriginaisR :: PedirNovoId -> Handler Html
getEditarOriginaisR cid = do
    pedirNovo <- runDB $ get404 cid
    (widget,_) <- generateFormPost (formPedirNovoR (Just pedirNovo))
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/formContato.lucius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/pedirOriginalEditar.hamlet")
        

postEditarOriginaisR :: PedirNovoId -> Handler Html
postEditarOriginaisR cid = do
    _ <- runDB $ get404 cid
    ((result,_),_) <-runFormPost (formPedirNovoR Nothing)
    case result of
        FormSuccess novoPeNovo -> do
            runDB $ replace cid novoPeNovo
            redirect ListPedidosOriginaisR
        _ -> redirect HomeR





