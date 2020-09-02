{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Scirate
import           Gui
import           Lens.Micro
import           Control.Concurrent.Async.Pool (mapConcurrently, withTaskGroup)
import           Configuration.Dotenv ( loadFile, defaultConfig, Config(..) )
import           System.Directory     ( createDirectoryIfMissing
                                      , getHomeDirectory
                                      )
import           System.FilePath      ((</>))
import           Data.Aeson           (encode, decode)
import           Data.Maybe           (fromJust)
import           Data.Semigroup       ((<>))
import           Options.Applicative  ( Parser
                                      , info
                                      , helper
                                      , execParser
                                      , (<*>)
                                      , value
                                      , help
                                      , flag
                                      , long
                                      , option
                                      , auto )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

data Mode
  = Resume
  | New

data Options = Options
  { mode       :: Mode
  , range      :: Int
  , browserCmd :: Text.Text
  }


opts :: Parser Options
opts =
  Options
    <$> flag Resume New
        ( long "new" <> help "Don't resume a previous session; start a new one." )
    <*> option auto
        ( long "range" <> help "If new, the `range` parameter for scirate." <> value 1 )
    <*> option auto
        ( long "browser-cmd" <> help "Command to open a browser." <> value "google-chrome" )

config :: FilePath -> Config
config homeDir =
  Config
    { configExamplePath = []
    , configOverride = False
    , configPath = [ homeDir </> ".env" ]
    }

main :: IO ()
main = do
  opts    <- execParser (info (helper <*> opts) mempty)
  dataDir <- flip (</>) ".scirate-cli" <$> getHomeDirectory 
  createDirectoryIfMissing True dataDir

  loadFile (config dataDir)

  let queryFilePath = dataDir </> "query.json"
      stateFilePath = dataDir </> "state.json"
      openLaterCmds = dataDir </> "openLater.sh"

  state <- case (mode opts) of
    New -> do
      --  1. Set up a scirate query
      --  2. Run it and collect papers
      q <- runScirateQuery ("https://scirate.com/?range=" <> show (range opts)) (range opts)

      --  3. Save the query (and the papers)
      BSL.writeFile queryFilePath (encode q)

      --
      let state = AppState 
                    { _papers       = q ^. papersFound
                    , _currentIndex = 0
                    , _scited       = []
                    , _ignored      = []
                    , _openingLater = []
                    , _actions      = []
                    }

      --  4. Run
      return state

    Resume -> do
      state <- fromJust . decode <$> BSL.readFile stateFilePath
      return state

  newState <- runGui state


  -- Write the 'openLater' file ...
  let toOpen    = map (mkCmd . (<>) "https://scirate.com/arxiv/" . _uid) (newState ^. openingLater)
      mkCmd url = (browserCmd opts) <> " " <> url <> " >/dev/null 2>&1 &"

  Text.writeFile openLaterCmds (Text.unlines toOpen)


  -- Run all the scitations
  _ <- withTaskGroup 10 $ \g -> mapConcurrently g scitePaper (newState ^. scited)


  BSL.writeFile stateFilePath (encode newState)
