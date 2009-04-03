{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell , FlexibleInstances,
             UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

{-|
 - This demonstrates the use of the auth library.  This code compiles,
 - but is not really a working demo.  It needs a lot of work.
 -}
module Main where

import Control.Concurrent
import Control.Monad
import Happstack.Server
import Happstack.State

import Happstack.Auth

impl = msum
  [ dir "login" $ msum [methodSP GET $ (fileServe ["login.html"] ".")
                       ,methodSP POST $ withData $ loginHandler loginGood loginBad]
  , dir "newuser" $ methodSP POST $ withData newUserPage
  , dir "view" $ withDataFn (liftM Just (readCookieValue "sid") `mplus` return Nothing) viewPage
  , dir "list" userListPage

  ,dir "logout" $ cookieR $ logoutHandler $ seeOther "/" (toResponse "Logged out")

  , anyRequest $ ok $ toResponse "Sorry, couldn't find a matching handler" ]

loginBad = ok $ toResponse $ "Invalid login"
loginGood = seeOther "/" (toResponse "Logged in")

regNoMatch = ok $ toResponse $ "Passwords did not match"
regExists = ok $ toResponse $ "User already exists."
regGood = seeOther "/" (toResponse "User created.")

newUserPage (NewUserInfo user pass1 pass2)
  | pass1 == pass2 = anyRequest $ checkAndAdd regExists regGood (Username user) pass1
  | otherwise = anyRequest $ regNoMatch

viewPage (Just sid) = anyRequest $ do
  ses <- query $ (GetSession $ sid)
  ok $ toResponse $ "Cookie value: " ++ (maybe "not logged in" show (ses :: Maybe SessionData))

viewPage Nothing =
  anyRequest $ ok $ toResponse $ "Not logged in"

userListPage = anyRequest $ do u <- query ListUsers; ok $ toResponse $ "Users: " ++ (show u)

entryPoint :: Proxy AuthState
entryPoint = Proxy

main = do 
  control <- startSystemState entryPoint
  tid <- forkIO $ simpleHTTP nullConf impl
  waitForTermination
  putStrLn "Shutting down..."
  killThread tid
  shutdownSystem control
  putStrLn "Shutdown complete"

