{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Scirate where
  
import Lens.Micro.TH

import           Data.List           (zipWith5)
import qualified Data.Text           as Text
import qualified Text.HTML.DOM       as H
import           Text.XML.Cursor     (attributeIs, content, element,
                                      fromDocument,
                                      ($//), (&/), (&//), ($/), (&|))


data Paper = Paper
  { _title      :: !Text.Text
  , _abstract   :: !Text.Text
  , _authors    :: ![Text.Text]
  , _scites     :: !Int
  , _categories :: ![Text.Text]
  } deriving (Show, Eq)
makeLenses ''Paper


-- loadPapers :: IO [Paper]
loadPapers = do
  doc <- H.readFile "../a.html"
  
  -- TODO: This could probably be cleaned up.
  let cursor         = fromDocument doc
      papers         = cursor $// attributeIs "class" "papers" &/ element "li"
      titlesOf c     = map asString $ c $// attributeIs "class" "title" &/ element "a"
      abstractsOf c  = map asString $ c $// attributeIs "class" "abstract"
      authorsOf c    = map clean    $ c $// attributeIs "class" "authors"  &// element "a"
      scitesOf c     = map asString $ c $// attributeIs "class" "scites-count"  &// element "button"
      categoriesOf c = map clean    $ c $// attributeIs "class" "uid" &/ element "a"

      clean x    = x $// content &| Text.strip
      asString   = Text.intercalate " " . clean

      nonEmpty f papers = map head . filter (not . null) $ map f papers

      stripCommas = Text.dropAround (\c -> c == ',')

      final = zipWith5 Paper
                (nonEmpty titlesOf papers)
                (nonEmpty abstractsOf papers)
                (map (map (stripCommas . head)) (map authorsOf papers))
                (map (read . Text.unpack) $ nonEmpty scitesOf papers)
                (map (map head) (map categoriesOf papers))
  
  return final
