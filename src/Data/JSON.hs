module JSON
  (
  ) where

data VPMConfig = VPMConfig
    { use_lockfile :: Bool
    , vpm_version :: String
    } deriving (Show)
