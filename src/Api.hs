{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.Aeson
-- import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Data.List as List

import Util (lookUpEnvWithDefault)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> Capture "userid" Int :> Get '[JSON] User

-- data SortBy = Age | Name

data User = User {
    name :: String,
    age :: Int,
    email :: String
} deriving (Eq, Show, Generic)

instance ToJSON User

users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk"
  , User "Albert Einstein" 136 "ae@mc2.org"
  ]

getUsers :: Handler [User]
getUsers = return users

getById :: Int -> Handler User
getById index = 
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
