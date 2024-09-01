{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import Data.Time.Clock (getCurrentTime)

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( Role(Role)
    , User (User, userEmail, userPassword, userRole, userTelegram)
    , Controller (Controller, controllerModel, controllerBrithday), Parameter (Parameter, parameterName, parameterSymbol, parameterMeasure, parameterMin, parameterMax)
    )

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => ReaderT SqlBackend m ()
fillDemoEn = do

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

    p1 <- insert $ Parameter { parameterName = "Temperature"
                             , parameterSymbol = "°C"
                             , parameterMeasure = "degree"
                             , parameterMin = -100
                             , parameterMax = 100
                             }

    p2 <- insert $ Parameter { parameterName = "Acidity"
                             , parameterSymbol = "pH"
                             , parameterMeasure = ""
                             , parameterMin = 0
                             , parameterMax = 14
                             }

    p3 <- insert $ Parameter { parameterName = "CO2 level"
                             , parameterSymbol = "CO2"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p4 <- insert $ Parameter { parameterName = "O2 level"
                             , parameterSymbol = "O2"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p5 <- insert $ Parameter { parameterName = "O3 level"
                             , parameterSymbol = "O3"
                             , parameterMeasure = "ppm"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p6 <- insert $ Parameter { parameterName = "Potential"
                             , parameterSymbol = "V"
                             , parameterMeasure = "volt"
                             , parameterMin = 0
                             , parameterMax = 1000000
                             }

    p7 <- insert $ Parameter { parameterName = "Illuminance"
                             , parameterSymbol = "lux"
                             , parameterMeasure = "illuminance"
                             , parameterMin = 0
                             , parameterMax = 100000
                             }

    p7 <- insert $ Parameter { parameterName = "Preasure"
                             , parameterSymbol = "mm"
                             , parameterMeasure = "preasure"
                             , parameterMin = 0
                             , parameterMax = 100000
                             }

    return ()
    
