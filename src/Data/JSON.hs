{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON
  (
  ) where

import Data.Aeson

data VPMConfig = VPMConfig
    { use_lockfile :: Bool
    , vpm_version :: String
    } deriving (Show)

instance FromJSON VPMConfig

instance ToJSON VPMConfig
