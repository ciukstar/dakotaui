{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home
  ( getHomeR, postLangR
  ) where

import Foundation
    ( Handler
    , widgetLogo, widgetMainMenu, widgetTheme, widgetLang, widgetSnackbar
    , widgetMainMenuTrigger
    , Route (StaticR, HomeR, LangR)
    , AppMessage
      ( MsgHome, MsgCrashes, MsgEnterYourQuery, MsgMountPointName
      , MsgTemperatureValue, MsgValueOfIndicator
      )
    )
    
import Model (mediae)

    
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_sensor_map_svg, js_echarts_min_js)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), newIdent, addScript, getMessageRender
    , setLanguage, redirect, getMessages
    )
import Yesod.Core.Widget (setTitleI, addScriptRemote)
import Yesod.Form.Input (runInputPost, ireq)
import Yesod.Form.Fields (textField, urlField)


getHomeR :: Handler Html
getHomeR = do
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgHome
        idDialogMainMenu <- newIdent
        idArticleSensorMap <- newIdent
        idSensorMap <- newIdent
        idArticleIdicators <- newIdent
        idChart1 <- newIdent
        idChart2 <- newIdent
        idChart3 <- newIdent
        idButtonExportPdf <- newIdent
        addScript $ StaticR js_echarts_min_js
        addScriptRemote "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"
        $(widgetFile "homepage")


postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    back <- runInputPost $ ireq urlField "backlink"
    setLanguage lang
    redirect back
