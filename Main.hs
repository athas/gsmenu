{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  stable
-- Portability :  unportable
--
-- gsmenu, a generic grid-based menu.
--
-----------------------------------------------------------------------------

module Main (main) where

import Sindre.Main hiding (value, string, position)
import Sindre.Lib
import Sindre.Parser
import Sindre.Widgets
import Sindre.X11
import Sindre.KeyVal
import Sindre.Util

import Graphics.X11.Xlib hiding (refreshKeyboardMapping, Rectangle, textWidth, allocColor,
                                 textExtents)
import qualified Graphics.X11.Xft as Xft

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Either
import Data.List
import Data.Maybe
import Data.Traversable (traverse)
import Data.Word (Word8)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Printf

import System.Environment

-- | The main Sindre entry point.
main :: IO ()
main = case parseSindre emptyProgram "builtin" prog of
  Left e -> error $ show e
  Right prog' ->
    sindreMain prog' classMap' objectMap funcMap globMap =<< getArgs
  where prog = unlines [ "GUI { Vertically { Horizontally { Blank; label=Label(); Blank }; grid=Grid(); input=Input(minwidth=0) } }"
                       , "BEGIN { focus input; }"
                       , "<C-g> || <Escape> { exit 2 }"
                       , "stdin->lines(lines) { grid.insert(lines); }"
                       , "input.value->changed(from, to) { grid.filter(to) }"
                       , "<Right> || <C-f> { grid.east(); next }"
                       , "<Left> || <C-b> { grid.west(); next }"
                       , "<Up> || <C-p> { grid.north(); next }"
                       , "<Down> || <C-n> { grid.south(); next }"
                       , "<Return> { foo = grid.selected; print foo[\"value\"]; exit }"
                       , "<C-s> { grid.next(); next }"
                       , "<C-r> { grid.prev(); next }"
                       , "BEGIN { foo = grid.selected; if (foo) { label.label = foo[\"name\"] } }"
                       , "grid.selected->changed(from, to) { if (to) { label.label = to[\"name\"] } }" ]
        classMap' = M.insert "Grid" mkGrid classMap

data Element = Element { elementName     :: T.Text
                       , elementSubnames :: [T.Text]
                       , elementTags     :: [T.Text]
                       , elementFg       :: Xft.Color
                       , elementBg       :: Xft.Color
                       , elementValue    :: T.Text }

instance Show Element where
  show = show . elementName

data DisplayedElement = DisplayedElement { displayedName :: String
                                         , displayedSubnames :: [String]
                                         , displayedElement :: Element
                                         } deriving (Show)

match :: T.Text -> Element -> Bool
match f e = (T.toCaseFold f `T.isInfixOf`) `any`
            map T.toCaseFold (elementName e : elementSubnames e ++ elementTags e)

type TwoDPos = (Integer, Integer)
type TwoDElement = (TwoDPos, DisplayedElement)

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
diamondLayer 0 = [(0,0)]
diamondLayer n = concat [ zip [0..]      [n,n-1..1]
                        , zip [n,n-1..1] [0,-1..]
                        , zip [0,-1..]   [-n..(-1)]
                        , zip [-n..(-1)] [0,1..] ]

diamond :: (Enum a, Num a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: Integer -> Integer -> [TwoDPos]
diamondRestrict x y =
  filter (\(x',y') -> abs x' <= x && abs y' <= y) .
  map (\(x', y') -> (x', y')) .
  take 1000 $ diamond

data ElementGrid =
  ElementGrid { position :: TwoDPos
              , elements :: M.Map TwoDPos DisplayedElement }
  deriving (Show)

emptyGrid :: ElementGrid
emptyGrid = ElementGrid (0,0) M.empty

gridMove :: (TwoDPos -> TwoDPos) -> ElementGrid -> Maybe ElementGrid
gridMove f grid = const (grid { position = pos' })
                  <$> M.lookup pos' (elements grid)
  where pos' = f $ position grid

south :: ElementGrid -> Maybe ElementGrid
south = gridMove $ \(x,y) -> (x,y+1)
east :: ElementGrid -> Maybe ElementGrid
east = gridMove $ \(x,y) -> (x+1,y)
north :: ElementGrid -> Maybe ElementGrid
north = gridMove $ \(x,y) -> (x,y-1)
west :: ElementGrid -> Maybe ElementGrid
west = gridMove $ \(x,y) -> (x-1,y)

gridToList :: ElementGrid -> [TwoDElement]
gridToList = M.toList . elements

gridFromMap :: M.Map TwoDPos DisplayedElement -> TwoDPos -> ElementGrid
gridFromMap = flip ElementGrid

cellWidth :: Num a => a
cellWidth = 130
cellHeight :: Num a => a
cellHeight = 50
cellPadding :: Num a => a
cellPadding = 10

data Grid = Grid { gridElems      :: [Element]
                 , gridSelElems   :: [Element]
                 , gridFilter     :: T.Text
                 , gridElementMap :: ElementGrid
                 , gridVisual     :: VisualOpts
                 , gridRect       :: Rectangle
                 }

selection :: Grid -> Value
selection g = maybe falsity (asDict . displayedElement)
              $ M.lookup (position grid) $ elements grid
  where grid = gridElementMap g
        asDict e = Dict $ M.fromList
                   [ (unmold "name", unmold $ elementName e)
                   , (unmold "subnames", dict $ elementSubnames e)
                   , (unmold "tags", dict $ elementTags e)
                   , (unmold "value", unmold $ elementValue e)]
        dict = Dict . M.fromList . zip (map Number [0..]) . map unmold

gridBox :: Drawer -> Element -> SindreX11M DisplayedElement
gridBox d e@Element { elementName = text, elementSubnames = subs } = do
  d' <- io $ (`setBgColor` elementBg e) <=<
             (`setFgColor` elementFg e) $ d
  let theight  = Xft.height $ drawerFont d'
      sheights = map (const theight) subs
      room     = cellHeight-2*cellPadding-theight
      (_, subs') =
        fromMaybe (0::Int, []) $
        find ((<=room) . fst) $
        reverse $ zip (map sum $ inits sheights) (inits $ map T.unpack subs)
      line = stopText (drawerFont d') (cellWidth-(2*cellPadding))
  liftM3 DisplayedElement
           (line $ T.unpack text)
           (mapM line subs')
           (return e)

stopText :: Xft.Font -> Dimension -> String -> SindreX11M String
stopText f mw =
  shrinkWhile (reverse . inits)
  (\n -> (> fi mw) <$> fst <$> textExtents f n)

shrinkWhile :: Monad m => ([a] -> [[a]])
            -> ([a] -> m Bool)
            -> [a] -> m [a]
shrinkWhile sh p x = sw $ sh x
    where sw [n] = return n
          sw [] = return []
          sw (n:ns) = do cond <- p n
                         if cond
                            then sw ns
                            else return n

recomputeMap :: Drawer -> ObjectM Grid SindreX11M ()
recomputeMap d = do
  Rectangle _ _ rwidth rheight <- gets gridRect
  let restriction ss cs = (ss/cs-1)/2 :: Double
      restrictX = floor $ restriction (fi rwidth) cellWidth
      restrictY = floor $ restriction (fi rheight) cellHeight
      coords = diamondRestrict restrictX restrictY
      buildGrid = traverse (back . gridBox d) . M.fromList . zip coords
  oldsel <- gets selection
  elmap <- buildGrid =<< gets gridSelElems
  modify $ \s -> s { gridElementMap = gridFromMap elmap (0,0) }
  newsel <- gets selection
  when (oldsel /= newsel) $
    changed "selected" oldsel newsel

needRecompute :: ObjectM Grid SindreX11M ()
needRecompute = do modify $ \s -> s { gridElementMap = emptyGrid
                                    , gridRect = Rectangle 0 0 0 0 }
                   fullRedraw

updateRect :: Rectangle -> Drawer -> ObjectM Grid SindreX11M ()
updateRect r1 d = do r2 <- gets gridRect
                     unless (r1 == r2) $ do
                       modify $ \s -> s { gridRect = r1 }
                       recomputeMap d

methInsert :: T.Text -> ObjectM Grid SindreX11M ()
methInsert vs = case partitionEithers $ parser vs of
                  (e:_,_) -> fail $ "Parse error on Grid element: " ++ e
                  ([],els) -> do
                    els' <- back $ sequence els
                    modify $ \s ->
                      s { gridElems = gridElems s ++ els'
                        , gridSelElems = gridSelElems s ++
                          filter (match $ gridFilter s) els' }
                    needRecompute
  where parser = map parseElement . T.lines

methRemove :: (Element -> Bool) -> ObjectM Grid SindreX11M ()
methRemove f = do modify $ \s ->
                    s { gridElems = filter f $ gridElems s
                      , gridSelElems = filter f $ gridSelElems s }
                  needRecompute

methRemoveByValue :: T.Text -> ObjectM Grid SindreX11M ()
methRemoveByValue k = methRemove $ (/=k) . elementValue

methRemoveByName :: T.Text -> ObjectM Grid SindreX11M ()
methRemoveByName k = methRemove $ (/=k) . elementName

methClear :: ObjectM Grid SindreX11M ()
methClear = modify $ \s -> s { gridElementMap = emptyGrid
                             , gridElems      = []
                             , gridSelElems   = [] }

methFilter :: T.Text -> ObjectM Grid SindreX11M ()
methFilter f = do changeFields [("selected", unmold . selection)] $ \s ->
                      return s { gridSelElems = filter (match f) $ gridElems s }
                  needRecompute

methNext :: ObjectM Grid SindreX11M ()
methPrev :: ObjectM Grid SindreX11M ()
(methNext, methPrev) = (circle next, circle prev)
    where circle f = do changeFields [("selected", unmold . selection)] $ \s ->
                            case gridElementMap s of
                              elems@ElementGrid{position=(x,y)} ->
                                return s { gridElementMap =
                                             fromMaybe elems $ f x y elems
                                         }
                        redraw
          next (-1) y | y >= 0 = south <=< south <=< east
          next x y = case (compare x 0, compare y 0) of
                       (EQ, EQ) -> south
                       (EQ, GT) -> east <=< north
                       (EQ, LT) -> west <=< south
                       (LT, GT) -> south <=< east
                       (LT, EQ) -> south <=< east
                       (LT, LT) -> west <=< south
                       (GT, GT) -> east <=< north
                       (GT, _) -> north <=< west
          prev 0 1 = north
          prev x y = case (compare x 0, compare y 0) of
                       (EQ, EQ) -> const Nothing
                       (EQ, GT) -> west <=< north <=< north
                       (EQ, LT) -> east <=< south
                       (LT, GT) -> west <=< north
                       (LT, EQ) -> north <=< east
                       (LT, LT) -> north <=< east
                       (GT, LT) -> east <=< south
                       (GT, _) -> south <=< west

move :: (ElementGrid -> Maybe ElementGrid) -> ObjectM Grid SindreX11M ()
move d = do changeFields [("selected", unmold . selection)] $ \s ->
              return s { gridElementMap = fromMaybe (gridElementMap s)
                                          $ d $ gridElementMap s
                       }
            redraw

instance Object SindreX11M Grid where
    fieldSetI _ _ = return $ Number 0
    fieldGetI "selected" = selection <$> get
    fieldGetI "elements" = Dict <$> M.fromList <$>
                           zip (map Number [1..]) <$>
                           map (unmold . elementValue) <$> gridSelElems <$> get
    fieldGetI _ = return $ Number 0
    callMethodI "insert" = function methInsert
    callMethodI "removeValue" = function methRemoveByValue
    callMethodI "removeName" = function methRemoveByName
    callMethodI "clear"  = function methClear
    callMethodI "filter" = function methFilter
    callMethodI "next" = function methNext
    callMethodI "prev" = function methPrev
    callMethodI "north" = function $ move north
    callMethodI "south" = function $ move south
    callMethodI "west" = function $ move west
    callMethodI "east" = function $ move east
    callMethodI m = fail $ "Unknown method '" ++ m ++ "'"

instance Widget SindreX11M Grid where
    composeI = return (Unlimited, Unlimited)
    drawI = drawing gridVisual $ \r d fd -> do
      updateRect r d
      elems <- gets gridElementMap
      let update (p,e) x y cw ch = do
            d' <- io $ (`setBgColor` elementBg (displayedElement e)) <=<
                       (`setFgColor` elementFg (displayedElement e)) $ d
            let drawbox | p == position elems = drawWinBox fd
                        | otherwise = drawWinBox d'
            drawbox e x y cw ch
      back $ updatingBoxes update r elems

updatingBoxes :: (TwoDElement
                  -> Position -> Position
                  -> Dimension -> Dimension
                  -> SindreX11M ())
              -> Rectangle -> ElementGrid -> SindreX11M [Rectangle]
updatingBoxes f (Rectangle origx origy w h) egrid = do
  let w'  = origx + div (w-cellWidth) 2
      h'  = origy + div (h-cellHeight) 2
      proc ((x,y), t) = do
        f ((x,y), t)
          (fi $ w'+x*cellWidth) (fi $ h'+y*cellHeight)
          cellWidth cellHeight
        return $ Rectangle (fi w'+x*cellWidth) (h'+y*cellHeight)
                           (cellWidth+1) (cellHeight+1)
  mapM proc $ gridToList egrid

drawWinBox :: Drawer
           -> DisplayedElement
           -> Position -> Position -> Dimension -> Dimension
           -> SindreX11M ()
drawWinBox d e x y cw ch = do
  io $ bg d fillRectangle x y cw ch
  io $ fg d drawRectangle x y cw ch
  let subs = displayedSubnames e
      theight  = Xft.height $ drawerFont d
      sheights = map (const $ Xft.height $ drawerFont d) subs
      sheight = length subs * fi theight
      x' = x+cellPadding
      y' = y+(fi ch-fi sheight-fi theight) `div` 2
      ys = map (+(y'+theight)) $ scanl (+) 0 sheights
      putline voff =
        drawText (drawerFgColor d) (drawerFont d) x' voff theight
  _ <- putline y' $ displayedName e
  zipWithM_ putline ys subs

mkGrid :: Constructor SindreX11M
mkGrid r [] = do
  visual <- visualOpts r
  return $ NewWidget
    Grid { gridElems      = []
         , gridSelElems   = []
         , gridFilter     = T.empty
         , gridElementMap = emptyGrid
         , gridVisual     = visual
         , gridRect       = Rectangle 0 0 0 0
         }
mkGrid  _ _ = error "Grids do not have children"

parseElement :: T.Text -> Either String (SindreX11M Element)
parseElement = parseKV textelement
  where textelement = el
                      <$?> ([], values $ T.pack "tags")
                      <||> values (T.pack "name")
                      <|?> (Nothing, Just <$> value (T.pack "fg"))
                      <|?> (Nothing, Just <$> value (T.pack "bg"))
                      <|?> (Nothing, Just <$> value (T.pack "value"))
        el tags (name:names) fgc bgc val =
          let (tfg, tbg) = tagColors $ map T.unpack tags
          in do mgr <- asks sindreXftMgr
                fgpix <- allocColor mgr $ maybe tfg T.unpack fgc
                bgpix <- allocColor mgr $ maybe tbg T.unpack bgc
                return Element { elementName = name
                               , elementSubnames = names
                               , elementTags = tags
                               , elementFg = fgpix
                               , elementBg = bgpix
                               , elementValue = fromMaybe name val }
        el _ _ _ _ _ = error "No name"

tagColors :: [String] -> (String, String)
tagColors ts =
  let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
      (r,g,b) = hsv2rgb (seed 83 `mod` 360,
                         fi (seed 191 `mod` 1000)/2500+0.4,
                         fi (seed 121 `mod` 1000)/2500+0.4)
  in ("white", '#' : concatMap (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b] )
    where s = show ts

twodigitHex :: Word8 -> String
twodigitHex = printf "%02x"
