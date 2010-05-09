----------------------------------------------------------------------------
-- |
-- Module      :  GSMenu.Font
-- Copyright   :  (c) 2007 Andrea Rossato and Spencer Janssen
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for abstracting a font facility over Core fonts and Xft
-- Taken from XMonad.Util.Font.
--
-----------------------------------------------------------------------------

module GSMenu.Font
    ( -- * Usage:
      -- $usage
      GSMenuFont(..)
    , initColor
    , initXMF
    , releaseXMF
    , initCoreFont
    , releaseCoreFont
    , initUtf8Font
    , releaseUtf8Font
    , Align (..)
    , stringPosition
    , textWidthXMF
    , textExtentsXMF
    , printStringXMF
    , stringToPixel
    , decodeInput
    , encodeOutput
    ) where

import Foreign
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Maybe

#ifdef XFT
import Data.List
import Graphics.X11.Xft
import Graphics.X11.Xrender
#endif

import Graphics.X11
import Graphics.X11.Xlib.Extras

import Codec.Binary.UTF8.String (encodeString, decodeString)

import GSMenu.Util


-- Hide the Core Font/Xft switching here
data GSMenuFont = Core FontStruct
               | Utf8 FontSet
#ifdef XFT
               | Xft  XftFont
#endif

-- | Get the 'Pixel' value for a named color
initColor :: Display -> String -> IO (Maybe Pixel)
initColor dpy c = catch
  (Just . color_pixel . fst <$> allocNamedColor dpy colormap c)
  (const $ return Nothing)
    where colormap = defaultColormap dpy (defaultScreen dpy)

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
stringToPixel :: (Functor m, MonadIO m) => Display -> String -> m Pixel
stringToPixel d s = fromMaybe fallBack <$> io getIt
    where getIt    = initColor d s
          fallBack = blackPixel d (defaultScreen d)


-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: MonadIO m => Display -> String -> m FontStruct
initCoreFont dpy s =
  io $ catch getIt fallBack
      where getIt    = loadQueryFont dpy s
            fallBack = const $ loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseCoreFont :: MonadIO m => Display -> FontStruct -> m ()
releaseCoreFont dpy = io . freeFont dpy

initUtf8Font :: MonadIO m => Display -> String -> m FontSet
initUtf8Font dpy s = do
  (_,_,fs) <- io $ catch getIt fallBack
  return fs
      where getIt    = createFontSet dpy s
            fallBack = const $ createFontSet dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseUtf8Font :: MonadIO m => Display -> FontSet -> m ()
releaseUtf8Font dpy = io . freeFontSet dpy

-- | When initXMF gets a font name that starts with 'xft:' it switches to the Xft backend
-- Example: 'xft: Sans-10'
initXMF :: MonadIO m => Display -> String -> m GSMenuFont
initXMF dpy s =
#ifdef XFT
  if xftPrefix `isPrefixOf` s then
     do xftdraw <- io $ xftFontOpen dpy (defaultScreenOfDisplay dpy) (drop (length xftPrefix) s)
        return (Xft xftdraw)
  else
#endif
      liftM Utf8 $ initUtf8Font dpy s
#ifdef XFT
  where xftPrefix = "xft:"
#endif

releaseXMF :: MonadIO m => Display -> GSMenuFont -> m ()
#ifdef XFT
releaseXMF dpy (Xft xftfont) =
  io $ xftFontClose dpy xftfont
#endif
releaseXMF dpy (Utf8 fs) = releaseUtf8Font dpy fs
releaseXMF dpy (Core fs) = releaseCoreFont dpy fs


textWidthXMF :: MonadIO m => Display -> GSMenuFont -> String -> m Int
textWidthXMF _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidthXMF _   (Core fs) s = return $ fi $ textWidth fs s
#ifdef XFT
textWidthXMF dpy (Xft xftdraw) s = liftIO $ do
    gi <- xftTextExtents dpy xftdraw s
    return $ xglyphinfo_xOff gi
#endif

textExtentsXMF :: MonadIO m => GSMenuFont -> String -> m (Int32,Int32)
textExtentsXMF (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fi $ - (rect_y rl)
      descent = fi $ rect_height rl + fi (rect_y rl)
  return (ascent, descent)
textExtentsXMF (Core fs) s = do
  let (_,a,d,_) = textExtents fs s
  return (a,d)
#ifdef XFT
textExtentsXMF (Xft xftfont) _ = io $ do
  ascent  <- fi `fmap` xftfont_ascent  xftfont
  descent <- fi `fmap` xftfont_descent xftfont
  return (ascent, descent)
#endif

-- | String position
data Align = AlignCenter | AlignRight | AlignLeft | AlignRightOffset Int
                deriving (Show, Read)

-- | Return the string x and y 'Position' in a 'Rectangle', given a
-- 'FontStruct' and the 'Align'ment
stringPosition :: (Functor m, MonadIO m) => Display -> GSMenuFont -> Rectangle -> Align -> String -> m (Position,Position)
stringPosition dpy fs (Rectangle _ _ w h) al s = do
  width <- textWidthXMF dpy fs s
  (a,d) <- textExtentsXMF fs s
  let y = fi $ ((h - fi (a + d)) `div` 2) + fi a;
      x = case al of
            AlignCenter -> fi (w `div` 2) - fi (width `div` 2)
            AlignLeft   -> 1
            AlignRight  -> fi (w - (fi width + 1));
            AlignRightOffset offset -> fi (w - (fi width + 1)) - fi offset;
  return (x,y)

printStringXMF :: (Functor m, MonadIO m) => Display -> Drawable
               -> GSMenuFont -> GC -> String -> String
               -> Position -> Position -> String  -> m ()
printStringXMF d p (Core fs) gc fc bc x y s = io $ do
    setFont d gc $ fontFromFontStruct fs
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    drawImageString d p gc x y s
printStringXMF d p (Utf8 fs) gc fc bc x y s = io $ do
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    io $ wcDrawImageString d p fs gc x y s
#ifdef XFT
printStringXMF dpy drw fs@(Xft font) gc fc bc x y s = do
  let screen   = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual   = defaultVisualOfScreen screen
  bcolor <- stringToPixel dpy bc
  (a,d)  <- textExtentsXMF fs s
  gi <- io $ xftTextExtents dpy font s
  io $ setForeground dpy gc bcolor
  io $ fillRectangle dpy drw gc (x - fi (xglyphinfo_x gi))
                                (y - fi a)
                                (fi $ xglyphinfo_xOff gi)
                                (fi $ a + d)
  io $ withXftDraw dpy drw visual colormap $
         \draw -> withXftColorName dpy visual colormap fc $
                   \color -> xftDrawString draw color font x y s
#endif

decodeInput :: String -> String
decodeInput = decodeString

encodeOutput :: String -> String
encodeOutput = encodeString
