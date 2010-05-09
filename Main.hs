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

module Main () where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.List
import Data.Word (Word8)

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xinerama

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Text.Printf

import GSMenu.Config
import GSMenu.Pick
import GSMenu.Util

data AppConfig a = AppConfig {
      cfg_complex   :: Bool
    , cfg_display   :: String
    , cfg_enumerate :: Bool
    , cfg_gpconfig  :: GPConfig a
  }

defaultConfig :: AppConfig a
defaultConfig = AppConfig {
                  cfg_complex   = False
                , cfg_display   = ""
                , cfg_enumerate = False
                , cfg_gpconfig  = defaultGPConfig
                }

main :: IO ()
main = do
  opts  <- getOpt RequireOrder options <$> getArgs
  dstr  <- getEnv "DISPLAY" `catch` const (return "")
  let cfg = defaultConfig { cfg_display = dstr }
  case opts of
    (opts', [], []) -> runWithCfg =<< foldl (>>=) (return cfg) opts'
    (_, nonopts, errs) -> do 
              mapM_ (hPutStrLn stderr . ("Junk argument: " ++)) nonopts
              usage <- usageStr
              hPutStrLn stderr $ concat errs ++ usage
              exitFailure

runWithCfg :: AppConfig String -> IO ()
runWithCfg cfg = do 
  dpy   <- setupDisplay $ cfg_display cfg
  let screen = defaultScreenOfDisplay dpy
  elems <- reader stdin valuer
  rect  <- findRectangle dpy (rootWindowOfScreen screen)
  sel   <- gpick dpy screen rect (cfg_gpconfig cfg) elems
  case sel of
    Left reason     -> err reason >> exitWith (ExitFailure 1)
    Right Nothing   -> exitWith $ ExitFailure 2
    Right (Just el) -> putStr el >> exitSuccess
    where reader
           | cfg_complex cfg = readElementsC "stdin"
           | otherwise       = readElements
          valuer
           | cfg_enumerate cfg = const show
           | otherwise         = \ s _ -> s

setupDisplay :: String -> IO Display
setupDisplay dstr =
  openDisplay dstr `Prelude.catch` \_ ->
    error $ "Cannot open display \"" ++ dstr ++ "\"."

findRectangle :: Display -> Window -> IO Rectangle
findRectangle dpy rootw = do
  (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
  let hasPointer rect = fi x >= rect_x rect &&
                        fi (rect_width rect) + rect_x rect > fi x &&
                        fi y >= rect_y rect &&
                        fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find hasPointer <$> getScreenInfo dpy

readElements :: MonadIO m => Handle 
             -> (String -> Integer -> a)
             -> m [Element a]
readElements h f = do
  str   <- io $ hGetContents h
  return $ zipWith mk (lines str) [0..]
      where mk line num = Element
                          { el_colors = ("black", "white")
                          , el_data   = f line num
                          , el_disp   = (line, [])
                          , el_tags   = [] }
                          
readElementsC :: MonadIO m => SourceName
              -> Handle
              -> (String -> Integer -> a)
              -> m [Element a]
readElementsC sn h f = do
  str   <- io $ hGetContents h
  case parseElements sn str of
    Left  e   -> error $ show e
    Right els -> return $ zipWith mk els [0..]
        where mk elm num = elm {
                el_data = f (fst $ el_disp elm) num }

type GSMenuOption a = OptDescr (AppConfig a -> IO (AppConfig a))

options :: [GSMenuOption a]
options = [optHelp, optVersion, optDisplay, optComplex, optEnumResult,
           optFont, optSubFont, optInputFont]

inGPConfig :: (String -> GPConfig a -> GPConfig a)
            -> String -> AppConfig a -> IO (AppConfig a)
inGPConfig f arg cfg = return $ cfg { cfg_gpconfig = f arg (cfg_gpconfig cfg) }

optHelp :: GSMenuOption a
optHelp = Option "h" ["help"]
          (NoArg $ \_ -> do
             hPutStrLn stderr =<< usageStr
             exitSuccess)
          "Display this help screen."
          
usageStr :: IO String
usageStr = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " " ++ versionString
  return $ usageInfo header options
  
optVersion :: GSMenuOption a
optVersion = Option "v" ["version"]
             (NoArg $ \_ -> do 
                hPutStrLn stderr ("gsmenu " ++ versionString ++ ".")
                hPutStrLn stderr "Copyright (C) Troels Henriksen."
                exitSuccess)
             "Print version number."
             
versionString :: String
versionString = "1.1-dev"
             
optDisplay :: GSMenuOption a
optDisplay = Option "d" ["display"]
             (ReqArg (\arg cfg -> return $ cfg { cfg_display = arg }) "dpy" )
             "Specify the X display to connect to."
             
optComplex :: GSMenuOption a
optComplex = Option "c" ["complex"]
             (NoArg (\cfg -> return $ cfg { cfg_complex = True }) )
             "Use complex input format."

optEnumResult :: GSMenuOption a
optEnumResult = Option "e" ["enumerate"]
                (NoArg (\cfg -> return $ cfg { cfg_enumerate = True }) )
                "Print the result as the (zero-indexed) element number."

optFont :: GSMenuOption a
optFont = Option [] ["font"]
          (ReqArg (inGPConfig $ \arg gpc -> gpc { gp_font = arg }) "font")
          "The font used for printing names of elements"

optSubFont :: GSMenuOption a
optSubFont = Option [] ["subfont"]
             (ReqArg (inGPConfig $ \arg gpc -> gpc { gp_subfont = arg}) "font")
             "The font used for printing extra lines in elements"

optInputFont :: GSMenuOption a
optInputFont = Option [] ["inputfont"]
               (ReqArg (inGPConfig $ \arg gpc -> gpc { gp_inputfont = arg}) "font")
               "The font used for the input field"
               
parseElements :: SourceName -> String -> Either ParseError [Element a]
parseElements = parse $ many element <* eof

blankElem :: Element a
blankElem = Element {
              el_colors = ("black", "white")
            , el_data   = error "Element without data"
            , el_disp   = error "Element without display"
            , el_tags   = []
            }

tagColors :: [String] -> (String, String)
tagColors ts =
  let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
      (r,g,b) = hsv2rgb (seed 83 `mod` 360,
                         fi (seed 191 `mod` 1000)/2500+0.4,
                         fi  (seed 121 `mod` 1000)/2500+0.4)
  in ("white", '#' : concatMap (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b] )
    where s = show ts

twodigitHex :: Word8 -> String
twodigitHex = printf "%02x"

element :: GenParser Char u (Element a)
element = do kvs <- kvPair `sepBy1` realSpaces <* spaces
             let (fg, bg) = tagColors $ tags kvs
             foldM procKv blankElem { el_colors = (fg, bg) } kvs
    where tags (("tags",ts):ls) = ts ++ tags ls
          tags ((_,_):ls)       = tags ls
          tags []               = []
          procKv elm ("name", val : more) =
            return elm { el_disp = (val, more) }
          procKv _   ("name", _) = badval "name"
          procKv elm ("fg", [val]) =
            return elm {
              el_colors = (val, snd $ el_colors elm) }
          procKv _   ("fg", _) = badval "fg"
          procKv elm ("bg", [val]) =
            return elm {
              el_colors = (fst $ el_colors elm, val) }
          procKv _   ("bg", _) = badval "bg"
          procKv elm ("tags",val) =
            return elm { el_tags = el_tags elm ++ filter (/="") val }
          procKv _ (k, _) = nokey k
          badval = parserFail . ("Bad value for field " ++) . quote
          nokey  = parserFail . ("Unknown key " ++) . quote

kvPair :: GenParser Char u (String, [String])
kvPair =
  pure (,) <*> (many1 alphaNum <* realSpaces <* char '=' <* realSpaces)
           <*> many1 (value <* realSpaces)

value :: GenParser Char u String
value = char '"' *> escapedStr

escapedStr :: GenParser Char u String
escapedStr = do
  s <- many $ noneOf "\"\n"
  (    try (string "\"\"" *> pure ((s++"\"")++) <*> escapedStr)
   <|> try (string "\"" *> return s))

realSpaces :: GenParser Char u String
realSpaces = many $ char ' '
