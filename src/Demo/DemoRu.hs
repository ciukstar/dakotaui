{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoRu (fillDemoRu) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.Time.Clock (getCurrentTime)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userRole, userTelegram)
    , Controller (Controller, controllerBrithday, controllerModel)
    , Parameter
      ( Parameter, parameterName, parameterSymbol, parameterMeasure
      , parameterMin, parameterMax
      )
    )

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => ReaderT SqlBackend m ()
fillDemoRu = do

    now <- liftIO getCurrentTime
    
    r1 <- insert $ Role "Администратор"

    r2 <- insert $ Role "Пользователь"

    pass1 <- liftIO $ saltPass "admin"
    insert_ $ User { userEmail = "admin@mail.ru"
                   , userPassword = Just pass1
                   , userRole = r1
                   , userTelegram = "1231233123"
                   }

    pass2 <- liftIO $ saltPass "user1"
    insert_ $ User { userEmail = "user1@mail.ru"
                   , userPassword = Just pass2
                   , userRole = r2
                   , userTelegram = "1231233123"
                   }

    pass3 <- liftIO $ saltPass "oleg"
    insert_ $ User { userEmail = "oleg@mail.ru"
                   , userPassword = Just pass3
                   , userRole = r1
                   , userTelegram = "1231233123"
                   }

    c1 <- insert $ Controller { controllerBrithday = now
                              , controllerModel = 1
                              }

    p1 <- insert $ Parameter { parameterName = "Температура"
                             , parameterSymbol = "°C"
                             , parameterMeasure = "градус"
                             , parameterMin = -100
                             , parameterMax = 100
                             }

    p2 <- insert $ Parameter { parameterName = "Кислотность"
                             , parameterSymbol = "pH"
                             , parameterMeasure = ""
                             , parameterMin = 0
                             , parameterMax = 14
                             }

    p3 <- insert $ Parameter { parameterName = "Уровень CO2"
                             , parameterSymbol = "CO2"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p4 <- insert $ Parameter { parameterName = "Уровень O2"
                             , parameterSymbol = "O2"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p5 <- insert $ Parameter { parameterName = "Уровень O3"
                             , parameterSymbol = "O3"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p6 <- insert $ Parameter { parameterName = "Потенциал"
                             , parameterSymbol = "V"
                             , parameterMeasure = "volt"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p7 <- insert $ Parameter { parameterName = "Освещенность"
                             , parameterSymbol = "lux"
                             , parameterMeasure = "освещенность"
                             , parameterMin = 0
                             , parameterMax = 100000
                             }

    p7 <- insert $ Parameter { parameterName = "Давление"
                             , parameterSymbol = "mm"
                             , parameterMeasure = "давление"
                             , parameterMin = 0
                             , parameterMax = 100000
                             }

    return ()
    
