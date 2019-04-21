{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp

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

getById :: Int -> User
getById index = users !! index

server1 :: Server UserAPI
server1 = return users
     :<|> \index -> return (getById index)

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

start :: IO ()
start = run 3000 app1
