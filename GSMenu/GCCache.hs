-----------------------------------------------------------------------------
-- |
-- Module      :  GSMenu.GCCache
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  stable
-- Portability :  unportable
--
-- Cache for creating and retrieving Xlib GC values.
--
-----------------------------------------------------------------------------

module GSMenu.GCCache ( getGC
                      , GCCache
                      , empty
                      , allGCs
                      , freeCache
                      , GCParams(..)
  ) where

import Control.Monad.Trans

import qualified Data.Map as M
import Data.Maybe

import Graphics.X11.Xlib

import GSMenu.Font
import GSMenu.Util

data GCParams = GCParams {
      gc_fg :: String 
    } deriving (Eq, Ord)

type InnerCache = M.Map GCParams GC

newtype GCCache = GCCache (M.Map Drawable InnerCache)

empty :: GCCache
empty = GCCache M.empty

innerCache :: GCCache -> Drawable -> InnerCache
innerCache (GCCache c) d =
  fromMaybe M.empty $ M.lookup d c

cachedGC :: GCCache -> Drawable -> GCParams -> Maybe GC
cachedGC cache d p = M.lookup p $ innerCache cache d

setCached :: GCCache -> Drawable -> GCParams -> GC -> GCCache
setCached cache@(GCCache c) d p gc =
  GCCache $ M.insert d inner' c
  where inner  = innerCache cache d
        inner' = M.insert p gc inner

getGC :: MonadIO m => Display -> Screen -> GCCache -> Drawable -> GCParams -> m (GC, GCCache)
getGC dpy screen cache d p =
  case cachedGC cache d p of
    Just x -> return (x, cache)
    Nothing -> do
      x <- getGC'
      return (x, setCached cache d p x)
    where getGC' = io $ do
            gc <- createGC dpy d
            fgc <- initColor dpy $ gc_fg p
            case fgc of
              Just fgc' -> setForeground dpy gc fgc'
              Nothing -> do err $ "Bad color " ++ gc_fg p
                            setForeground dpy gc wp
            return gc
          wp = whitePixelOfScreen screen

allGCs :: GCCache -> [GC]
allGCs (GCCache c) = concatMap M.elems $ M.elems c

freeCache :: Display -> GCCache -> IO ()
freeCache dpy = mapM_ (freeGC dpy) . allGCs
