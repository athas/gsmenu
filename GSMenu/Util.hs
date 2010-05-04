-----------------------------------------------------------------------------
-- |
-- Module      :  GSMenu.Util
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  stable
-- Portability :  portable
--
-- Various utility bits and pieces.
--
-----------------------------------------------------------------------------

module GSMenu.Util
    ( io
    , fi
    , err
    , upcase
    , downcase
    , hsv2rgb
    ) where

import Control.Monad.Trans

import Data.Char

import System.IO

-- | Short-hand for 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Short-hand for 'liftIO . hPutStrLn stderr'
err :: MonadIO m => String -> m ()
err = io . hPutStrLn stderr

-- | Short-hand for 'map toUpper'
upcase :: String -> String
upcase = map toUpper

-- | Short-hand for 'map toLower'
downcase :: String -> String
downcase = map toLower

-- | Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = div h 60 `mod` 6 :: Integer
        f = fi h/60 - fi hi :: Fractional a => a
        q = v * (1-f)
        p = v * (1-s)
        t = v * (1-(1-f)*s)
    in case hi of
         0 -> (v,t,p)
         1 -> (q,v,p)
         2 -> (p,v,t)
         3 -> (p,q,v)
         4 -> (t,p,v)
         5 -> (v,p,q)
         _ -> error "The world is ending. x mod a >= a."
