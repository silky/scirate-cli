{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Scirate where
  
import Lens.Micro.TH

import           Conduit (runConduit, (.|))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.List (zipWith6)
import           Lens.Micro
import           GHC.Generics
import           Data.Maybe (isJust, fromJust)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import           Text.Read (readMaybe)
import            System.Environment (getEnv)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Text.HTML.DOM       as H
import           Text.XML            (Document, Node(..), Element(..), Name(..))
import           Text.XML.Cursor     (attributeIs, node, content, element,
                                      fromDocument, Axis, attribute,
                                      ($//), (&/), (&//), ($/), (&|))


data Paper = Paper
  { _title      :: !Text.Text
  , _abstract   :: !Text.Text
  , _authors    :: ![Text.Text]
  , _scites     :: !Int
  , _categories :: ![Text.Text]
  , _uid        :: !Text.Text
  } deriving (Show, Eq, Generic)
makeLenses ''Paper

instance ToJSON   Paper
instance FromJSON Paper


data ScirateQuery = ScirateQuery
  { _date        :: !UTCTime
  , _initialUrl  :: !String
  , _visitedUrls :: ![String]
  , _papersFound :: ![Paper]
  } deriving (Show, Generic)
makeLenses ''ScirateQuery

instance ToJSON   ScirateQuery
instance FromJSON ScirateQuery


getPages :: Document -> Int -> [String]
getPages doc range = 
  let cursor = fromDocument doc
      pages :: [Maybe Int]
      pages  = cursor $// attributeIs "class" "pagination" 
                      &/ element "a" 
                      &/ content
                      &| readMaybe . Text.unpack
      pages'  = map fromJust $ filter isJust pages
      minPage = minimum pages'
      maxPage = maximum pages'
 
      mkPageQuery k = "https://scirate.com/?page=" ++ show k ++ "&range=" ++ show range
      pageQueries   = map mkPageQuery [minPage .. maxPage]
   in pageQueries



-- | Select only those element nodes containing the given attribute key/value pair. (As
--   compared to `attributeIs` which _says_ it does "containing"; but actually
--   doesn't.
attributeContains :: Name -> Text.Text -> Axis
attributeContains n v c =
    case node c of
        NodeElement (Element _ as _) -> [ c | maybe False (Text.isInfixOf v) (Map.lookup n as) ]
        _                            -> []


loadPapers :: Document -> [Paper]
loadPapers doc =
  let cursor         = fromDocument doc
      papers         = cursor $// attributeIs "class" "papers" &/ element "li"
      titlesOf c     = map asString $ c $// attributeIs       "class" "title"        &/  element "a"
      abstractsOf c  = map asString $ c $// attributeContains "class" "abstract"
      authorsOf c    = map clean    $ c $// attributeIs       "class" "authors"      &// element "a"
      scitesOf c     = map asString $ c $// attributeIs       "class" "scites-count" &// element "button"
      categoriesOf c = map clean    $ c $// attributeIs       "class" "uid"          &/  element "a"
      uidOf c        = map uidAttr  $ c $// attributeIs       "class" "scite-toggle"

      uidAttr  = head . attribute "data-paper-uid"
      clean x  = x $// content &| Text.strip
      asString = Text.intercalate " " . clean

      nonEmpty f papers = map head . filter (not . null) $ map f papers

      stripCommas = Text.dropAround (\c -> c == ',')

      final = zipWith6 Paper
                (nonEmpty titlesOf papers)
                (nonEmpty abstractsOf papers)
                (map (map (stripCommas . head)) (map authorsOf papers))
                (map (read . Text.unpack) (nonEmpty scitesOf papers))
                (map (map head) (map categoriesOf papers))
                (nonEmpty uidOf papers)
   in final


cookie :: String -> Cookie
cookie cookieValue = Cookie
                { cookie_name             = "_scirate3_session"
                , cookie_value            = BS.pack cookieValue
                , cookie_expiry_time      = future
                , cookie_domain           = "scirate.com"
                , cookie_path             = "/"
                , cookie_creation_time    = past
                , cookie_last_access_time = past
                , cookie_persistent       = False
                , cookie_host_only        = False
                , cookie_secure_only      = False
                , cookie_http_only        = True
                }
  where
    past   = UTCTime (ModifiedJulianDay 56200)  (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)


asDocument :: String 
           -> IO (Document)
asDocument url = do
  putStrLn $ "Querying ... " <> url

  request'    <- parseRequest url
  manager     <- newManager   tlsManagerSettings
  cookieValue <- getEnv "SCIRATE_COOKIE"

  let request = request' { cookieJar = Just $ createCookieJar [cookie cookieValue] }

  doc <- runResourceT $ do
      response <- http request manager
      runConduit $ responseBody response .| H.sinkDoc

  return doc


-- | Kicks off the querying of all the data.
runScirateQuery :: String -> Int -> IO (ScirateQuery)
runScirateQuery url range = do
  now  <- getCurrentTime
  doc0 <- asDocument url
  
  let pages = getPages doc0 range

  allDocs <- ((:) doc0) <$> mapM asDocument pages

  let allPapers = concat $ map loadPapers allDocs
      scirate   = ScirateQuery
                    { _date        = now
                    , _initialUrl  = url
                    , _visitedUrls = pages
                    , _papersFound = allPapers
                    }
  return scirate
