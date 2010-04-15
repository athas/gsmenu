{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GPick.Pick
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  BSD-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module GPick.Pick
    ( GPConfig(..)
    , Element(..)
    , defaultGPConfig
    , KeyMap
    , TwoDPosition
    , gpick
    ) where

import Data.Maybe
import Data.Bits
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Data.List as L
import qualified Data.Map as M

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xshape

import GPick.Font
import GPick.Util

data GPConfig a = GPConfig {
      gp_bordercolor  :: String
    , gp_cellheight   :: Dimension
    , gp_cellwidth    :: Dimension
    , gp_cellpadding  :: Dimension
    , gp_font         :: String
    , gp_keymap       :: KeyMap a
    , gp_originFractX :: Double
    , gp_originFractY :: Double
}

type KeyMap a = M.Map (KeyMask,KeySym) (TwoD a ())

type TwoDPosition = (Integer, Integer)
data Element a = Element { el_colors :: (String, String)
                         , el_data   :: a
                         , el_disp   :: String
                         , el_tags   :: [String] }

type TwoDElement a    = (TwoDPosition, Element a)
type TwoDElementMap a = [(TwoDPosition, Element a)]

data ElemPane = ElemPane {
      ep_width     :: Dimension
    , ep_height    :: Dimension
    , ep_win       :: Window
    , ep_shapemask :: Pixmap
    , ep_maskgc    :: GC
    , ep_unmaskgc  :: GC
    , ep_textgc    :: GC
    }

data TwoDState a = TwoDState { td_curpos     :: TwoDPosition
                             , td_elementmap :: TwoDElementMap a
                             , td_colorcache :: M.Map (String, String)
                                                (GC, String, String)
                             }

data TwoDConf a = TwoDConf {
      td_elempane :: ElemPane
    , td_gpconfig :: GPConfig a
    , td_display  :: Display
    , td_screen   :: Screen
    , td_font     :: GPickFont
    , td_elems    :: [Element a]
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

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
-- FIXME remove nub
diamondLayer n = let ul = [ (x,n-x) | x <- [0..n] ]
        in nub $ ul ++ (map (negate *** id) ul) ++
           (map (negate *** negate) ul) ++
           (map (id *** negate) ul)

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

drawWinBox :: Display -> Window -> GPickFont -> String
           -> (String, String) -> String -> Dimension
           -> Position -> Position -> Dimension -> Dimension
           -> TwoD a ()
drawWinBox dpy win font bc (fg,bg) text cp x y cw ch = do
  (gc, fg', bg') <- procColors dpy (fg, bg)
  textgc <- asks (ep_textgc . td_elempane)
  io $ do
    bordergc <- createGC dpy win
    Just bordercolor <- initColor dpy bc
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

updateAllElements :: TwoD a ()
updateAllElements = do
  els <- gets td_elementmap
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
  updateElements els

updateElements :: TwoDElementMap a -> TwoD a ()
updateElements elementmap = do
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

handle :: (KeySym, t) -> Event -> TwoD a (Maybe a)
handle (ks,_) (KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress && ks == xK_Escape = return Nothing
    | t == keyPress && ks == xK_Return = do
       (TwoDState { td_curpos = pos, td_elementmap = elmap }) <- get
       return $ (el_data . snd) <$> findInElementMap pos elmap
    | t == keyPress = do
        keymap <- asks (gp_keymap . td_gpconfig)
        maybe eventLoop (>>eventLoop)
          . M.lookup (m',ks) $ keymap
  where m' = cleanMask m

handle _ (ButtonEvent { ev_event_type = t, ev_x = x, ev_y = y })
    | t == buttonRelease = do
        elmap <- gets td_elementmap
        ch    <- asks (gp_cellheight . td_gpconfig)
        cw    <- asks (gp_cellwidth . td_gpconfig)
        w     <- asks (ep_width . td_elempane)
        h     <- asks (ep_height . td_elempane)
        let gridX = fi $ (fi x - (w - cw) `div` 2) `div` cw
            gridY = fi $ (fi y - (h - ch) `div` 2) `div` ch
            el    = lookup (gridX,gridY) elmap
        maybe eventLoop (return . Just . el_data) el
    | otherwise = eventLoop

handle _ (ExposeEvent { }) = updateAllElements >> eventLoop

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


-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gpick :: Display -> Screen -> Rectangle -> GPConfig a
      -> [Element a] -> IO (Maybe a)
gpick _ _ _ _ [] = return Nothing
gpick dpy screen rect gpconfig ellist = do
  let rwidth  = rect_width rect
      rheight = rect_height rect
  ep@ElemPane { ep_win = win } <- mkElemPane dpy screen rect
  status <- grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
  grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
  font <- initXMF dpy (gp_font gpconfig)
  selectedElement <-
    if status /= grabSuccess then return Nothing
    else do
      let restriction ss cs = (ss/fi (cs gpconfig)-1)/2 :: Double
          restrictX = floor $ restriction (fi rwidth) gp_cellwidth
          restrictY = floor $ restriction (fi rheight) gp_cellheight
          originPosX = floor $ ((gp_originFractX gpconfig) - (1/2)) * 2 * fromIntegral restrictX
          originPosY = floor $ ((gp_originFractY gpconfig) - (1/2)) * 2 * fromIntegral restrictY
          coords = diamondRestrict restrictX restrictY originPosX originPosY
          elmap  = zip coords ellist
      evalTwoD (updateAllElements >> eventLoop)
        TwoDState { td_curpos     = head coords
                  , td_elementmap = elmap
                  , td_colorcache = M.empty }
        TwoDConf { td_elempane = ep
                 , td_gpconfig = gpconfig
                 , td_display  = dpy
                 , td_screen   = screen
                 , td_font     = font
                 , td_elems    = ellist }
  freeElemPane dpy ep
  releaseXMF dpy font
  return selectedElement

move :: (Integer, Integer) -> TwoD a ()
move (dx, dy) = do
  state <- get
  let elmap    = td_elementmap state
      (ox, oy) = td_curpos state
      newPos   = (ox+dx, oy+dy)
      newSelectedEl = findInElementMap newPos elmap
  when (isJust newSelectedEl) $ do
    put state { td_curpos =  newPos }
    updateElements
      (catMaybes [ findInElementMap (ox, oy) elmap
                 , newSelectedEl])

defaultGPConfig :: GPConfig a
defaultGPConfig = GPConfig {
                    gp_bordercolor = "white"
                  , gp_cellheight = 50
                  , gp_cellwidth = 130
                  , gp_cellpadding = 10
                  , gp_font = "xft:Sans-8"
                  , gp_keymap = defaultGPNav
                  , gp_originFractX = 1/2
                  , gp_originFractY = 1/2
                  }


defaultGPNav :: KeyMap a
defaultGPNav = M.fromList
    [((0,xK_Left)           ,move (-1,0))
    ,((controlMask,xK_b)    ,move (-1,0))
    ,((0,xK_Right)          ,move (1,0))
    ,((controlMask,xK_f)    ,move (1,0))
    ,((0,xK_Down)           ,move (0,1))
    ,((controlMask,xK_n)    ,move (0,1))
    ,((0,xK_Up)             ,move (0,-1))
    ,((controlMask,xK_p)    ,move (0,-1))
    ]
