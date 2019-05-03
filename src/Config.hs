{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Prelude                 hiding ( readFile )
import           GHC.Generics
import           Data.Aeson
import           Data.ByteString.Lazy           ( readFile )
import           System.Directory
import           System.FilePath                ( (</>) )
import           Data.Maybe                     ( fromJust )

data Config = Config {
    passwordsPath :: FilePath
} deriving (Generic, Show)

instance FromJSON Config

-- | Parses the config file. May throw
getConfig :: IO Config
getConfig = do
    home     <- getHomeDirectory
    contents <- readFile (home </> ".pconfig.json")
    return $ fromJust $ decode contents
