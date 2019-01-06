module Config
    ( Config(..)
    , Options(..)
    , configFromOptions
    )
where

data Config = Config
  { logger :: String
  , includeDependencies :: Bool
  , includeCustomScripts :: Bool
  }

data Options = Options
  { includeDependencies :: Bool
  , includeCustomScripts :: Bool
  }

configFromOptions Options { includeDependencies, includeCustomScripts } =
    Config
        { logger               = "BasicLogger"
        , includeDependencies  = includeDependencies
        , includeCustomScripts = includeCustomScripts
        }
