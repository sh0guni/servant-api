module Util where

import Data.Maybe (fromMaybe)
import System.Environment
import Text.Read

lookUpEnvWithDefault :: Read a => String -> a -> IO a
lookUpEnvWithDefault name d = do
  value <- lookupEnv name
  let parsed = value >>= readMaybe
  return $ fromMaybe d parsed
