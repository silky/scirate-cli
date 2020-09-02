{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Gui where

import           Brick ((<=>), (<+>))
import           Data.Aeson
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           Scirate
import qualified Brick                 as B
import qualified Brick.BChan           as B
import qualified Brick.Focus           as B
import qualified Brick.Forms           as B
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as B
import qualified Brick.Widgets.List    as B
import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Graphics.Vty          as V
import qualified Graphics.Vty         as V
import qualified Graphics.Vty.Input.Events as B


-- TODO:
--  - Make the UI smoother with buttons


-- Stolen from diagrams.
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- Using `PatternSynonyms`
pattern VtyC :: Char -> [B.Modifier] -> B.BrickEvent n e
pattern VtyC c ms = B.VtyEvent (V.EvKey (V.KChar c) ms)

pattern VtyE :: B.Key -> [B.Modifier] -> B.BrickEvent n e
pattern VtyE k ms = B.VtyEvent (V.EvKey k ms)

-- Stolen from https://hackage.haskell.org/package/lens-3.2/docs/src/Data-List-Lens.html#~%3A
-- to append to a setter that refers to a list.
(~:) :: a -> ASetter s t [a] [a] -> s -> t
n ~: l = over l (n :)

instance B.Splittable [] where
  splitAt = List.splitAt

data Name
  = Scited
  | Ignored
  | OpeningLater
  deriving (Eq, Ord, Show, Generic)

instance ToJSON   Name
instance FromJSON Name

data AppState = AppState
  { _papers       :: ![Paper]
  , _currentIndex :: !Int
  , _scited       :: ![Paper]
  , _ignored      :: ![Paper]
  , _openingLater :: ![Paper]
  -- | For undo
  , _actions      :: ![Name]
  } deriving (Show, Generic)
makeLenses ''AppState

instance ToJSON   AppState
instance FromJSON AppState


paperPanel :: [Paper] -> B.Widget Name
paperPanel []            = B.center $
  B.txt "All caught up!"
paperPanel (nextPaper:_) =
  ( paperTitle <=>
      ( paperAbstract <+> paperMeta ) # B.center
  ) # B.center
  where
    paperTitle
      = (B.str " " <+> (B.withAttr "title" $ B.txt title') <+> B.str " ")

    paperAbstract = ( B.txtWrap abstract' <=> B.fill ' ' )
                      # B.padBottom (B.Pad 1) 
                      # B.padTop    (B.Pad 1) 
                      # B.padLeft   (B.Pad 2) 
                      # B.padRight  (B.Pad 2) 
                      # B.borderWithLabel (B.withAttr "scites" $ B.txt (" Scites: " <> Text.pack (show $ nextPaper ^. scites) <> " "))
                      # B.hLimit 118
                      # B.vLimit 25
                      # B.padAll 1
    paperMeta = ( B.txt "Authors:"
                <=>
                B.txt (Text.intercalate "\n" (map ((<>) " - ") authors'))
                <=>
                B.txt "\n"
                <=>
                B.txt "Categories:"
                <=>
                B.txt (Text.intercalate "\n" (map ((<>) " - ") categories'))
                <=>
                B.txt "\n"
                <=>
                B.txt (nextPaper ^. uid)
                <=>
                B.fill ' ')
                  # B.padBottom (B.Pad 1) 
                  # B.padTop    (B.Pad 1) 
                  # B.padLeft   (B.Pad 2) 
                  # B.padRight  (B.Pad 2) 
                  # B.borderWithLabel (B.withAttr "scites" $ B.txt (" arXiv:" <> nextPaper ^. uid <> " "))
                  # B.hLimit 30
                  # B.vLimit 25
                  # B.padAll 1
    abstract'   = nextPaper ^. abstract
    authors'    = nextPaper ^. authors
    categories' = nextPaper ^. categories
    title'      = nextPaper ^. title


draw :: AppState
     -> [B.Widget Name]
draw state =
  [ 
    B.hBorderWithLabel (
      B.txt $ " Remaining: " 
        <> (Text.pack (show . length $ state ^. papers))
        <> " "
    )
    <=>
    paperPanel (state ^. papers)
    <=> 
    statePanels
    <=>
    B.hBorderWithLabel (B.txt help)
  ]
    where
      help = " scirate-cli (Keys: n - No action, s - Scite, o - Open later, u - Undo, q - Quit) "
      ignored'      = listPapers Ignored    (state ^. ignored)
                        # B.borderWithLabel (B.txt (" ~ No action ~ "))
                        # B.hLimit 50

      scited'       = listPapers Scited     (state ^. scited)
                        # B.borderWithLabel (B.txt (" ~ Scite ~ "))
                        # B.hLimit 50

      openingLater' = listPapers OpeningLater (state ^. openingLater)
                        # B.borderWithLabel   (B.txt (" ~ Open Later ~ "))
                        # B.hLimit 50

      statePanels   = ignored' <+> scited' <+> openingLater'
                        # B.padLeft  (B.Pad 1)
                        # B.padRight (B.Pad 1)
                        # B.vLimit 20
                        # B.hCenter


listPapers :: Name -> [Paper] -> B.Widget Name
listPapers n papers
  = B.renderList (const B.txt) False paperList
    where
      paperList = B.list n (map _title papers) 1


app :: B.App AppState e Name
app = B.App 
  { B.appDraw         = draw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = eventHandler
  , B.appStartEvent   = return
  , B.appAttrMap      = const style
  }


style :: B.AttrMap
style = B.attrMap V.defAttr
  [ ("title",  V.withStyle (B.fg V.brightGreen) V.underline)
  ]


updateList f a state = newState
  where
    (nextPaper:remainingPapers) = state ^. papers
    newState = state & nextPaper ~: f
                     & papers .~ remainingPapers
                     & a ~: actions


scite :: AppState -> AppState
scite = updateList scited Scited


ignore :: AppState -> AppState
ignore = updateList ignored Ignored


openLater :: AppState -> AppState
openLater state = newState
  where
    remainingPapers@(nextPaper:_) = state ^. papers

    newState 
      | not (null (state ^. openingLater)) &&
          head (state ^. openingLater) == nextPaper = state
      | otherwise
          = state & nextPaper ~: openingLater
                  & papers .~ remainingPapers
                  & OpeningLater ~: actions


undo :: AppState -> AppState
undo state =
    newState
  where
    newState
      | null (state ^. actions) = state
      | otherwise               = undoAction (head (state ^. actions))

    undoAction Scited  = 
      let (p:ps) = state ^. scited
       in state & scited .~ ps
                & actions .~ tail (state ^. actions)
                & p       ~: papers

    undoAction Ignored  = 
      let (p:ps) = state ^. ignored
       in state & ignored .~ ps
                & actions .~ tail (state ^. actions)
                & p       ~: papers

    undoAction OpeningLater = 
      let (p:ps) = state ^. openingLater
       in state & openingLater .~ ps
                & actions .~ tail (state ^. actions)


eventHandler :: AppState 
             -> B.BrickEvent Name e 
             -> B.EventM Name (B.Next (AppState))
eventHandler s ev = do
  let papersRemain = length (s ^. papers) > 0
  case ev of
      B.VtyEvent (V.EvResize {})     -> B.continue s
      B.VtyEvent (V.EvKey V.KEsc []) -> B.halt s
      --
      VtyC 'q' _ -> B.halt s
      VtyC 'u' _ -> (B.continue . undo) s
      VtyC 's' _ | papersRemain -> (B.continue . scite) s
      VtyC ' ' _ | papersRemain -> (B.continue . ignore) s    -- <Space>: "No action".
      VtyC 'n' _ | papersRemain -> (B.continue . ignore) s    -- n: "No action".
      VtyC 'i' _ | papersRemain -> (B.continue . ignore) s    -- i: "No action" (was "Ignore"; hard to forget!)
      VtyC 'o' _ | papersRemain -> (B.continue . openLater) s -- Open Later (this one DOESN'T advance the list.)
      --
      _ -> B.continue s


runGui :: AppState -> IO AppState
runGui state = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  finalState <- B.customMain initialVty buildVty Nothing app state

  return finalState
