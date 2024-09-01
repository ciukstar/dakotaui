{-# LANGUAGE TemplateHaskell #-}

module Handler.Users (getUsersR) where

import Foundation
    ( Handler
    , widgetLogo, widgetTheme, widgetLang, widgetMainMenu, widgetMainMenuTrigger
    , Route (StaticR, HomeR, LangR, DataR)
    , DataR (UsersR)
    , AppMessage
      ( MsgUsers, MsgUpdateMap
      )
    )

import Settings.StaticFiles (img_logo_svg, img_sensor_map_svg)
import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, newIdent)
import Settings (widgetFile)


getUsersR :: Handler Html
getUsersR = do
    defaultLayout $ do
        setTitleI MsgUsers
        idDialogMainMeu <- newIdent
        idArticleSensorMap <- newIdent
        idSensorMap <- newIdent
        $(widgetFile "data/users/users")
