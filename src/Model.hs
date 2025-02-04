{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE InstanceSigs               #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }


mediae :: [(Text,Text)]
mediae = [("s","small"),("m","medium"),("l","large")] :: [(Text,Text)]

langs :: [(Text,Text)]
langs = [("ru","RU"),("en","EN")]

msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"

keyThemeMode :: Text
keyThemeMode = "dakotaui_theme_mode"
