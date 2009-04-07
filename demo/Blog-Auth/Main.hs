{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell , FlexibleInstances,
             FlexibleContexts, UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State (modify,put,get,gets,MonadState)
import Data.Generics hiding ((:+:))
import Happstack.Auth
import Happstack.Server
import Happstack.State

data Post = Post {
  postTitle :: String,
  postAuthor :: Username,
  postBody :: String
} deriving (Read,Show,Ord,Eq,Typeable,Data)

instance Version Post
$(deriveSerialize ''Post)

data BlogState = BlogState {
  postDB :: [Post]
} deriving (Read,Show,Ord,Eq,Typeable,Data)

instance Version BlogState
$(deriveSerialize ''BlogState)

instance Component BlogState where
  type Dependencies BlogState = AuthState :+: End
  initialValue = BlogState []

addPost :: (MonadState BlogState m) => Post -> m ()
addPost p = modify $ (\s -> BlogState $ p:(postDB s))

getPosts :: (MonadReader BlogState m) => m [Post]
getPosts = asks postDB
  
$(mkMethods ''BlogState ['addPost, 'getPosts])

redir url = seeOther url (toResponse "")

impl = msum
  [ dir "new" $ withSession newPostHandlers (redir "/login")
  , dir "newuser" $ methodSP POST $ newUserHandler exists noMatch regGood
  , dir "login" $ withSession (\_ -> redir "/") $
      msum [methodSP GET $ (fileServe ["login.html"] ".")
           ,methodSP POST $ loginHandler loginGood loginBad]
  , dir "logout" $ logoutHandler $ redir "/"
  , methodSP GET viewPostsHandler
  ]

newPostHandlers ses = msum [methodSP GET $ fileServe ["new_post.html"] "."
                           ,methodSP POST $ addPostHandler ses]

loginGood = redir "/new"
loginBad = ok $ toResponse $ "Invalid login"
exists = anyRequest $ ok $ toResponse $ "Username was invalid or already exists."
noMatch = anyRequest $ ok $ toResponse $ "Passwords did not match"
regGood = redir "/new"

addPostHandler ses = do
  (Just title) <- getDataFn $ look "title"
  (Just body) <- getDataFn $ look "body"
  update $ AddPost (Post title (sesUsername ses) body)
  redir "/"

viewPostsHandler = do
  posts <- query $ GetPosts
  case posts of
    [] -> ok $ toResponse "No posts yet"
    otherwise -> ok $ toResponse $ unlines $ map show posts

entryPoint :: Proxy BlogState
entryPoint = Proxy

main = do 
  control <- startSystemState entryPoint
  tid <- forkIO $ simpleHTTP nullConf impl
  waitForTermination
  putStrLn "Shutting down..."
  killThread tid
  shutdownSystem control
  putStrLn "Shutdown complete"
