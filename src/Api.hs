{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Data.List as List
import Data.List.Split (splitOn)
import Control.Monad.IO.Class (liftIO)

import Util (lookUpEnvWithDefault)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "userid" Int :> Get '[JSON] User

-- data SortBy = Age | Name

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

data User = User {
    name :: String,
    age :: Int,
    email :: String
} deriving (Eq, Show, Generic)

instance ToJSON User

listToUser :: [String] -> User
listToUser (name:age:email:_) = User name (read age) email

getUsers :: Handler [User]
getUsers = do
  contents <- liftIO (readFile "users.txt")
  return $ map (listToUser . splitOn ",") $ lines contents

getById :: Int -> Handler User
getById index = do
  users <- getUsers
  if index >= 0 && List.length users > index
     then return (users !! index)
     else throwError err404 { errBody = "Invalid user id" }

server1 :: Server UserAPI
server1 = getUsers
     :<|> getById

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

start :: IO ()
start = do
  port <- lookUpEnvWithDefault "PORT" 3000
  putStrLn $ "Server listening on port " ++ show port
  run port app1
