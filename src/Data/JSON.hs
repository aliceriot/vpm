{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON
  (
  ) where

import Data.Aeson
import GHC.Generics

-- import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

data VPMConfig = VPMConfig
    { use_lockfile :: Bool
    , vpm_version :: String
    } deriving (Show, Generic)

instance FromJSON VPMConfig

instance ToJSON VPMConfig

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile
