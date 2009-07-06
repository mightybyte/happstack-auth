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
    -- If logged in, then redirect to the root, otherwise display
    -- login page.
  [ dir "login" $ withSession (\_ -> rootRedirect) loginSpt

    -- Handle POSTed form for user registration
  , dir "newuser" $ methodSP POST $ withData newUserPage

    -- View the user's session information
  , dir "view" $ withSessionId viewPage

    -- Show list of users
  , dir "list" userListPage

    -- Log the user out
  , dir "logout" $ logoutHandler rootRedirect

    -- Matches http://domain.com/
    -- Provides basic links to the other pages.  Redirects to the
    -- login page if the users is not logged in.
  , withSession mainPage (seeOther "/login" $ toResponse "")]

rootRedirect :: ServerPart Response
rootRedirect = seeOther "/" $ toResponse ""

loginSpt = msum [methodSP GET $ (fileServe ["login.html"] ".")
                ,methodSP POST $ loginHandler loginGood loginBad]
mainPage ses = ok $ setHeader "Content-Type" "text/html" $ toResponse $
  "<html><body>"++
  "Logged in as "++(unUser $ sesUsername ses)++
  view++list++logout++
  "</body></html>"
  where view = "<div><a href=\"/view\">View session data</a></div>"
        list = "<div><a href=\"/list\">List users</a></div>"
        logout = "<div><a href=\"/logout\">Log out</a></div>"

loginBad = ok $ toResponse $ "Invalid login"
loginGood = rootRedirect

regNoMatch = anyRequest $ ok $ toResponse $ "Passwords did not match"
regExists = anyRequest $ ok $ toResponse $ "User already exists."

newUserPage (NewUserInfo user pass1 pass2)
  | pass1 == pass2 = checkAndAdd regExists rootRedirect (Username user) pass1
  | otherwise = regNoMatch

viewPage (Just sid) = anyRequest $ do
  ses <- query $ (GetSession $ sid)
  ok $ toResponse $ "Cookie value: " ++ (maybe "not logged in" show (ses :: Maybe SessionData))

viewPage Nothing = rootRedirect

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

