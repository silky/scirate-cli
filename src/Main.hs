module Main where

import           Scirate
import           Gui                  (runGui, AppState(..))
import           Lens.Micro
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

data Mode
  = Resume
  | New

data Options = Options
  { mode  :: Mode
  , range :: Int
  }


opts :: Parser Options
opts =
  Options
    <$> flag Resume New
        ( long "new" <> help "Don't resume a previous session; start a new one." )
    <*> option auto
        ( long "range" <> help "If new, the `range` parameter for scirate." <> value 1 )


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

  state <- case (mode opts) of
    New -> do
      -- putStrLn "Querying data from scirate ..."

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
      -- putStrLn "Resuming a previous session ..."
      state <- fromJust . decode <$> BSL.readFile stateFilePath
      return state

  newState <- runGui state

  BSL.writeFile stateFilePath (encode newState)
