module GMenu.Util
    ( io
    , fi
    , err
    , upcase
    , downcase
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
