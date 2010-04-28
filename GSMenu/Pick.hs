{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GSMenu.Pick
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  stable
-- Portability :  unportable
--
-- The main display and selection logic.
--
-----------------------------------------------------------------------------

module GSMenu.Pick
    ( GPConfig(..)
    , Element(..)
    , KeyMap
    , TwoDPosition
    , gpick
      
    , move
    , backspace
    , include
    , exclude
    ) where

import Data.Maybe
import Data.Bits
import Data.Char
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Data.List as L
import qualified Data.Map as M

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xshape

import GSMenu.Font
import GSMenu.Util

data GPConfig a = GPConfig {
      gp_bordercolor  :: String
    , gp_cellheight   :: Dimension
    , gp_cellwidth    :: Dimension
    , gp_cellpadding  :: Dimension
    , gp_font         :: String
    , gp_inputfont    :: String
    , gp_keymap       :: KeyMap a
    , gp_originFractX :: Double
    , gp_originFractY :: Double
}

type KeyMap a = M.Map (KeyMask,KeySym) (TwoD a ())

type TwoDPosition = (Integer, Integer)
data Element a = Element {
      el_colors :: (String, String)
    , el_data   :: a
    , el_disp   :: String
    , el_tags   :: [String]
    }

type TwoDElement a    = (TwoDPosition, Element (TwoD a (Maybe a)))
type TwoDElementMap a = [TwoDElement a]

data ElemPane = ElemPane {
      ep_width     :: Dimension
    , ep_height    :: Dimension
    , ep_win       :: Window
    , ep_shapemask :: Pixmap
    , ep_maskgc    :: GC
    , ep_unmaskgc  :: GC
    , ep_textgc    :: GC
    }

type TextBuffer = String

data TextPane a = TextPane {
      tp_win       :: Window
    , tp_bggc      :: GC
    , tp_fcolors   :: TwoDElementMap a -> (String, String)
    , tp_fieldgc   :: GC
    , tp_font      :: GSMenuFont
    , tp_lowerleft :: (Position, Position)
    , tp_width     :: Dimension
    }

data Filter = Include String
            | Exclude String
            | Running String

passes :: Filter -> Element a -> Bool
passes (Include s) elm =
  any (isInfixOf $ downcase s) strs
    where fields = map downcase (el_disp elm : el_tags elm)
passes (Exclude s) elm = not $ passes (Include s) elm
passes (Running s) elm = passes (Include s) elm

apply :: Filter -> [Element a] -> [Element a]
apply f = filter $ passes f

isRunning :: Filter -> Bool
isRunning (Running _) = True
isRunning _           = False

data FilterState a = FilterState {
      fl_filter :: Filter
    , fl_elmap  :: TwoDElementMap a
    , fl_elms   :: [Element a]
  }

data TwoDState a = TwoDState {
      td_curpos     :: TwoDPosition
    , td_colorcache :: M.Map (String, String)
                       (GC, String, String)
    , td_tbuffer    :: TextBuffer
    , td_filters    :: [FilterState a]
    }

data TwoDConf a = TwoDConf {
      td_elempane :: ElemPane
    , td_textpane :: TextPane a
    , td_gpconfig :: GPConfig a
    , td_display  :: Display
    , td_screen   :: Screen
    , td_font     :: GSMenuFont
    , td_elms     :: [Element a]
    , td_elmap    :: TwoDElementMap a
    }

newtype TwoD a b = TwoD (StateT (TwoDState a)
                         (ReaderT (TwoDConf a) IO) b)
    deriving (Monad, Functor, MonadState (TwoDState a),
              MonadReader (TwoDConf a), MonadIO)

instance Applicative (TwoD a) where
    (<*>) = ap
    pure = return

evalTwoD ::  TwoD a b -> TwoDState a -> TwoDConf a -> IO b
evalTwoD (TwoD m) s c = runReaderT (evalStateT m s) c

elements :: TwoD a [Element a]
elements = do
  s        <- get
  allelms  <- asks td_elms
  return $ fromMaybe allelms $ fl_elms <$> listToMaybe (td_filters s)

elementMap :: TwoD a (TwoDElementMap a)
elementMap = do
  s        <- get
  elmap  <- asks td_elmap
  return $ fromMaybe elmap $ fl_elmap <$> listToMaybe (td_filters s)

elementGrid :: [Element a] -> TwoD a (TwoDElementMap a)
elementGrid elms = do
  gpconfig <- asks td_gpconfig
  rwidth   <- asks (ep_width . td_elempane)
  rheight  <- asks (ep_height . td_elempane)
  let restriction ss cs = (ss/fi (cs gpconfig)-1)/2 :: Double
      restrictX = floor $ restriction (fi rwidth) gp_cellwidth
      restrictY = floor $ restriction (fi rheight) gp_cellheight
      originPosX = floor $ (gp_originFractX gpconfig - (1/2)) * 2 * fromIntegral restrictX
      originPosY = floor $ (gp_originFractY gpconfig - (1/2)) * 2 * fromIntegral restrictY
      coords = diamondRestrict restrictX restrictY originPosX originPosY
  return (zip coords $ map select elms)
  
select :: Element a -> Element (TwoD a (Maybe a))
select elm = elm { el_data = return $ Just $ el_data elm }

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
-- FIXME remove nub
diamondLayer n = 
  nub $ ul ++ (map (negate *** id) ul) ++
              (map (negate *** negate) ul) ++
              (map (id *** negate) ul)
    where ul = [ (x,n-x) | x <- [0..n] ]

diamond :: (Enum a, Num a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
diamondRestrict x y originX originY =
  L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
  map (\(x', y') -> (x' + originX, y' + originY)) .
  take 1000 $ diamond

findInElementMap :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findInElementMap pos = find ((== pos) . fst)

shrinkIt :: String -> [String]
shrinkIt "" = [""]
shrinkIt cs = cs : shrinkIt (init cs)

shrinkWhile :: Monad m => (String -> [String])
            -> (String -> m Bool)
            -> String -> m String
shrinkWhile sh p x = sw $ sh x
    where sw [n] = return n
          sw [] = return ""
          sw (n:ns) = do cond <- p n
                         if cond
                            then sw ns
                            else return n

drawWinBox :: Display -> Window -> GSMenuFont -> String
           -> (String, String) -> String -> Dimension
           -> Position -> Position -> Dimension -> Dimension
           -> TwoD a ()
drawWinBox dpy win font bc (fg,bg) text cp x y cw ch = do
  (gc, fg', bg') <- procColors dpy (fg, bg)
  textgc <- asks (ep_textgc . td_elempane)
  io $ do
    bordergc <- createGC dpy win
    bordercolor <- stringToPixel dpy bc
    setForeground dpy bordergc bordercolor
    fillRectangle dpy win gc x y cw ch
    drawRectangle dpy win bordergc x y cw ch
    stext <- shrinkWhile shrinkIt
             (\n -> do size <- textWidthXMF dpy font n
                       return $ size > fi (cw-fi (2*cp)))
             text
    printStringXMF dpy win font textgc fg' bg'
      (fi (x+fi cp)) (fi (y+fi (div ch 2))) stext
    freeGC dpy bordergc

drawBoxMask :: Display -> GC -> Pixmap -> Position
            -> Position -> Dimension -> Dimension -> IO ()
drawBoxMask dpy gc pm x y w h = do
  setForeground dpy gc 1
  fillRectangle dpy pm gc x y w h

procColors :: Display -> (String, String) -> TwoD a (GC, String, String)
procColors dpy col@(fg, bg) = do
  gcs <- gets td_colorcache
  case M.lookup col gcs of
    Just x -> return x
    Nothing -> do
      x <- procColors'
      modify $ \s -> s { td_colorcache = M.insert col x gcs }
      return x
    where procColors' = do
            screen <- asks td_screen
            win <- asks (ep_win . td_elempane)
            let wp = whitePixelOfScreen screen
                bp = blackPixelOfScreen screen
                badcol gc x = do err $ "Bad color " ++ x
                                 setBackground dpy gc bp
                                 setForeground dpy gc wp
                                 return (gc, "black", "white")
            io $ do
              gc <- io $ createGC dpy win
              fgc <- initColor dpy fg
              bgc <- initColor dpy bg
              case (fgc, bgc) of
                (Just fgc', Just bgc') -> do
                  setBackground dpy gc fgc'
                  setForeground dpy gc bgc'
                  return (gc, fg, bg)
                (Nothing, _) -> badcol gc fg
                (_, Nothing) -> badcol gc bg

updatingBoxes :: (TwoDElement a
                  -> Position -> Position
                  -> Dimension -> Dimension
                  -> TwoD a ())
              -> TwoDElementMap a -> TwoD a ()
updatingBoxes f els = do
  cellwidth  <- asks (gp_cellwidth  . td_gpconfig)
  cellheight <- asks (gp_cellheight . td_gpconfig)
  ElemPane { ep_width  = w
           , ep_height = h
           } <- asks td_elempane
  let w'  = div (w-cellwidth) 2
      h'  = div (h-cellheight) 2
      proc el@((x,y), _) =
        f el (fi $ fi w'+x*fi cellwidth)
             (fi $ fi h'+y*fi cellheight)
             (fi cellwidth) (fi cellheight)
  mapM_ proc els

redrawAllElements :: TwoD a ()
redrawAllElements = do
  els <- elementMap
  dpy <- asks td_display
  ElemPane { ep_width     = pw
           , ep_height    = ph
           , ep_win       = win
           , ep_shapemask = pm
           , ep_maskgc    = maskgc
           , ep_unmaskgc  = unmaskgc } <- asks td_elempane
  io $ fillRectangle dpy pm maskgc 0 0 pw ph
  let drawbox _ x y w h = io $ drawBoxMask dpy unmaskgc pm x y (w+1) (h+1)
  updatingBoxes drawbox els
  io $ xshapeCombineMask dpy win shapeBounding 0 0 pm shapeSet
  redrawElements els

redrawElements :: TwoDElementMap a -> TwoD a ()
redrawElements elementmap = do
  dpy     <- asks td_display
  font    <- asks td_font
  bc      <- asks (gp_bordercolor . td_gpconfig)
  padding <- asks (gp_cellpadding . td_gpconfig)
  win     <- asks (ep_win . td_elempane)
  curpos  <- gets td_curpos
  let update ((x,y),Element { el_colors = colors
                            , el_disp = text }) = do
        drawWinBox dpy win font bc colors' text padding
            where colors' | curpos == (x,y) =
                              ("black", "#faff69")
                          | otherwise = colors
  updatingBoxes update elementmap

updateTextInput :: TwoD a ()
updateTextInput = do
  dpy      <- asks td_display
  TextPane { tp_bggc = bggc, tp_win = win, tp_font = font 
           , tp_lowerleft = (x, y), tp_width = w, tp_fieldgc = fgc 
           , tp_fcolors = fcolors }
      <- asks td_textpane
  text  <- buildStr <$> gets (map fl_filter . td_filters)
  elmap <- elementMap
  (a,d) <- textExtentsXMF font text
  let h = max mh $ fi $ a + d
      (fg, bg) = fcolors elmap
  io $ do moveResizeWindow dpy win x (y-fi h) w h
          fillRectangle dpy win bggc 0 0 w h
          setForeground dpy fgc =<< stringToPixel dpy bg
          fillRectangle dpy win fgc margin 0 50 h
  printStringXMF dpy win font fgc fg bg margin (fi a) text
    where mh = 1
          margin = 20
          buildStr (Exclude str:fs) = buildStr fs ++ "¬" ++ str ++ "/"
          buildStr (Include str:fs) = buildStr fs ++ str ++ "/"
          buildStr (Running str:fs) = buildStr fs ++ take 1 (reverse str)
          buildStr _                = ""

changingState :: TwoD a b -> TwoD a b
changingState f =
  f <* redrawAllElements <* updateTextInput

pushFilter :: Filter -> TwoD a ()
pushFilter f = do
  elms' <- apply f <$> elements
  elmap <- elementGrid elms'
  modify $ \s -> s {
    td_filters = FilterState { fl_filter = f
                             , fl_elms   = elms'
                             , fl_elmap  = elmap } : td_filters s }

popFilter :: TwoD a ()
popFilter =
  modify $ \s -> s { td_filters = tail (td_filters s) }

topFilter :: TwoD a (Maybe Filter)
topFilter = do
  s <- get
  case td_filters s of
    (f:_) -> return $ Just $ fl_filter f
    _     -> return Nothing

input :: String -> TwoD a ()
input ""  = return ()
input str = changingState $ do
  f <- topFilter
  let str' = case f of
               Just (Running x) -> x ++ str
               _                -> str
  pushFilter $ Running str'

backspace :: TwoD a ()
backspace = changingState $ do
  f <- topFilter
  case f of
    Nothing -> return ()
    Just (Running _)   -> popFilter
    Just (Exclude str) -> runnings str
    Just (Include str) -> runnings str
    where runnings str = do
            popFilter
            mapM_ (pushFilter . Running) $ inits str

solidify :: (String -> Filter) -> TwoD a ()
solidify ff = changingState $ do
  f <- topFilter
  case f of
    Just (Running str) -> do
      modify $ \s -> s { td_filters =  dropRunning $ td_filters s }
      pushFilter (ff str)
    _                  -> return ()
    where dropRunning = dropWhile (isRunning . fl_filter)

exclude :: TwoD a ()
exclude = solidify Exclude

include :: TwoD a ()
include = solidify Include

move :: (Integer, Integer) -> TwoD a ()
move (dx, dy) = do
  state <- get
  elmap <- elementMap
  let (ox, oy) = td_curpos state
      newPos   = (ox+dx, oy+dy)
      newSelectedEl = findInElementMap newPos elmap
  when (isJust newSelectedEl) $ do
    put state { td_curpos =  newPos }
    redrawElements
      (catMaybes [ findInElementMap (ox, oy) elmap
                 , newSelectedEl])

eventLoop :: TwoD a (Maybe a)
eventLoop = do
  dpy <- asks td_display
  (keysym,string,event) <- io $ allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    (ks,s) <- if ev_event_type ev == keyPress
              then lookupString $ asKeyEvent e
              else return (Nothing, "")
    return (ks,s,ev)
  handle (fromMaybe xK_VoidSymbol keysym,string) event

cleanMask :: KeyMask -> KeyMask
cleanMask km = complement (numLockMask
                           .|. lockMask) .&. km
  where numLockMask :: KeyMask
        numLockMask = mod2Mask

handle :: (KeySym, String) -> Event -> TwoD a (Maybe a)
handle (ks,s) (KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress && ks == xK_Escape = return Nothing
    | t == keyPress && ks == xK_Return = do
      pos <- gets td_curpos
      elmap <- elementMap
      case lookup pos elmap of
        Nothing  -> eventLoop
        Just elm -> do maybe eventLoop (return . Just) =<< el_data elm
    | t == keyPress = do
      keymap <- asks (gp_keymap . td_gpconfig)
      maybe unbound id $ M.lookup (m',ks) $ keymap
      eventLoop
  where m' = cleanMask m
        unbound | not $ any isControl s = input s
                | otherwise = return ()

handle _ (ButtonEvent { ev_event_type = t, ev_x = x, ev_y = y })
    | t == buttonRelease = do
      elmap <- elementMap
      ch    <- asks (gp_cellheight . td_gpconfig)
      cw    <- asks (gp_cellwidth . td_gpconfig)
      w     <- asks (ep_width . td_elempane)
      h     <- asks (ep_height . td_elempane)
      let gridX = fi $ (fi x - (w - cw) `div` 2) `div` cw
          gridY = fi $ (fi y - (h - ch) `div` 2) `div` ch
      case lookup (gridX,gridY) elmap of
        Nothing  -> eventLoop
        Just elm -> do
          maybe eventLoop (return . Just) =<< el_data elm
    | otherwise = eventLoop

handle _ (ExposeEvent { ev_count = 0 }) = redrawAllElements >> eventLoop

handle _ _ = eventLoop

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow dpy s rw x y w h = do
  let visual   = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
      black    = blackPixelOfScreen s
      white    = whitePixelOfScreen s
  allocaSetWindowAttributes $ \attrs -> do
    set_override_redirect attrs True
    set_background_pixel attrs white
    set_border_pixel attrs black
    createWindow dpy rw x y w h 0 copyFromParent
                 inputOutput visual attrmask attrs

mkElemPane :: Display -> Screen -> Rectangle -> IO ElemPane
mkElemPane dpy screen rect = do
  let rootw   = rootWindowOfScreen screen
      rwidth  = rect_width rect
      rheight = rect_height rect
  win <- mkUnmanagedWindow dpy screen rootw
           (rect_x rect) (rect_y rect) rwidth rheight
  pm <- createPixmap dpy win rwidth rheight 1
  maskgc <- createGC dpy pm
  setForeground dpy maskgc 0
  fillRectangle dpy pm maskgc 0 0 rwidth rheight
  xshapeCombineMask dpy win shapeBounding 0 0 pm shapeSet
  unmaskgc <- createGC dpy pm
  setForeground dpy unmaskgc 1
  mapWindow dpy win
  selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  textgc <- createGC dpy rootw
  return ElemPane {
               ep_width     = fi rwidth
             , ep_height    = fi rheight
             , ep_win       = win
             , ep_shapemask = pm
             , ep_maskgc    = maskgc
             , ep_unmaskgc  = unmaskgc
             , ep_textgc    = textgc }

freeElemPane :: Display -> ElemPane -> IO ()
freeElemPane dpy ElemPane { ep_win      = win
                          , ep_maskgc   = maskgc
                          , ep_unmaskgc = unmaskgc
                          , ep_textgc   = textgc } = do
  unmapWindow dpy win
  destroyWindow dpy win
  mapM_ (freeGC dpy) [maskgc, unmaskgc, textgc]
  sync dpy False

fgGC :: Display -> Drawable -> (String, Pixel) -> IO GC
fgGC dpy drw (color, pixel) = do
  gc <- createGC dpy drw
  pix <- initColor dpy color
  setForeground dpy gc $ fromMaybe pixel pix
  return gc

mkTextPane :: Display -> Screen -> Rectangle -> GPConfig a
           -> IO (TextPane a)
mkTextPane dpy screen rect gpconfig = do
  let rootw   = rootWindowOfScreen screen
      wp      = whitePixelOfScreen screen
      bp      = blackPixelOfScreen screen
  win <- mkUnmanagedWindow dpy screen rootw
         (rect_x rect) (rect_y rect) 1 1
  bggc <- fgGC dpy win ("grey", wp)
  fgc  <- fgGC dpy win ("black", bp)
  font <- initXMF dpy (gp_inputfont gpconfig)
  let fcolors [] = ("white", "red")
      fcolors _  = ("white", "blue")
  mapWindow dpy win
  return TextPane { tp_win       = win
                  , tp_bggc      = bggc
                  , tp_fieldgc   = fgc
                  , tp_fcolors   = fcolors
                  , tp_font      = font
                  , tp_lowerleft = ( rect_x rect
                                   , rect_y rect + fi (rect_height rect)) 
                  , tp_width     = rect_width rect }

freeTextPane :: Display -> TextPane a -> IO ()
freeTextPane dpy TextPane { tp_win      = win
                          , tp_bggc     = bggc 
                          , tp_fieldgc  = fgc} = do
  unmapWindow dpy win
  destroyWindow dpy win
  mapM_ (freeGC dpy) [bggc, fgc]
  sync dpy False

-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gpick :: Display -> Screen -> Rectangle -> GPConfig a
      -> [Element a] -> IO (Maybe a)
gpick _ _ _ _ [] = return Nothing
gpick dpy screen rect gpconfig ellist = do
  let rwidth  = rect_width rect
      rheight = rect_height rect
  tp <- mkTextPane dpy screen rect gpconfig
  ep@ElemPane { ep_win = win } <- mkElemPane dpy screen rect
  status <- grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
  grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
  font      <- initXMF dpy (gp_font gpconfig)
  selectedElement <-
    if status /= grabSuccess then
       do err "Could not establish keyboard grab" 
          return Nothing
    else do
      let restriction ss cs = (ss/fi (cs gpconfig)-1)/2 :: Double
          restrictX = floor $ restriction (fi rwidth) gp_cellwidth
          restrictY = floor $ restriction (fi rheight) gp_cellheight
          originPosX = floor $ ((gp_originFractX gpconfig) - (1/2)) * 2 * fromIntegral restrictX
          originPosY = floor $ ((gp_originFractY gpconfig) - (1/2)) * 2 * fromIntegral restrictY
          coords = diamondRestrict restrictX restrictY originPosX originPosY
          boxelms = map select ellist
          elmap  = zip coords boxelms
      evalTwoD (do updateTextInput
                   redrawAllElements 
                   eventLoop)
        TwoDState { td_curpos     = head coords
                  , td_colorcache = M.empty 
                  , td_tbuffer    = "" 
                  , td_filters    = [] }
        TwoDConf { td_elempane  = ep
                 , td_textpane  = tp
                 , td_gpconfig  = gpconfig
                 , td_display   = dpy
                 , td_screen    = screen
                 , td_font      = font
                 , td_elmap     = elmap
                 , td_elms      = ellist }
  freeElemPane dpy ep
  freeTextPane dpy tp
  releaseXMF dpy font
  return selectedElement
