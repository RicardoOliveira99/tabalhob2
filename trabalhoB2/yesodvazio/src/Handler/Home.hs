{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")

getPedirPronto :: Handler Html
getPedirPronto = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        toWidgetHead $(luciusFile "templates/pedirPronto.lucius")
        $(whamletFile "templates/pedirPronto.hamlet")

getPedirOriginal :: Handler Html
getPedirOriginal = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        toWidgetHead $(luciusFile "templates/pedirOriginal.lucius")
        $(whamletFile "templates/pedirOriginal.hamlet")

getSobre :: Handler Html
getSobre = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/sobre.hamlet")

getFinalizar :: Handler Html
getFinalizar = do
    defaultLayout $ do
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/finalizar.hamlet")