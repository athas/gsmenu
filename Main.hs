module Main () where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.List

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xinerama

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import GSMenu.Pick
import GSMenu.Util

data AppConfig = AppConfig {
      cfg_complex   :: Bool
    , cfg_display   :: String
    , cfg_enumerate :: Bool
  }

defaultConfig :: AppConfig
defaultConfig = AppConfig {
                  cfg_complex   = False
                , cfg_display   = ""
                , cfg_enumerate = False
                }

main :: IO ()
main = do
  opts  <- getOpt RequireOrder options <$> getArgs
  dstr  <- getEnv "DISPLAY" `catch` (const $ return "")
  let cfg = defaultConfig { cfg_display = dstr }
  case opts of
    (opts', [], []) -> runWithCfg =<< foldl (>>=) (return cfg) opts'
    (_, nonopts, errs) -> do 
              mapM_ (hPutStrLn stderr) $ map ("Junk argument: " ++) nonopts
              usage <- usageStr
              hPutStrLn stderr $ concat errs ++ usage
              exitFailure


options :: [OptDescr (AppConfig -> IO AppConfig)]
options = [optHelp, optVersion, optDisplay, optComplex, optEnumResult]

optHelp :: OptDescr (AppConfig -> IO AppConfig)
optHelp = Option ['h'] ["help"]
          (NoArg $ \_ -> do
             hPutStrLn stderr =<< usageStr
             exitSuccess)
          "Display this help screen."
          
usageStr :: IO String
usageStr = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " " ++ versionString
  return $ usageInfo header options
  
optVersion :: OptDescr (AppConfig -> IO AppConfig)
optVersion = Option ['v'] ["version"]
             (NoArg $ \_ -> do 
                hPutStrLn stderr ("gsmenu " ++ versionString ++ ".")
                hPutStrLn stderr "Copyright (C) Troels Henriksen."
                exitSuccess)
             "Print version number."
             
versionString :: String
versionString = "1.0"
             
optDisplay :: OptDescr (AppConfig -> IO AppConfig)
optDisplay = Option ['d'] ["display"]
             (ReqArg (\arg cfg -> return $ cfg { cfg_display = arg }) "dpy" )
             "Specify the X display to connect to."
             
optComplex :: OptDescr (AppConfig -> IO AppConfig)
optComplex = Option ['c'] ["complex"]
             (NoArg (\cfg -> return $ cfg { cfg_complex = True }) )
             "Use complex input format."

optEnumResult :: OptDescr (AppConfig -> IO AppConfig)
optEnumResult = Option ['e'] ["enumerate"]
                (NoArg (\cfg -> return $ cfg { cfg_enumerate = True }) )
                "Print the result as the (zero-indexed) element number."

runWithCfg :: AppConfig -> IO ()
runWithCfg cfg = do 
  dpy   <- setupDisplay $ cfg_display cfg
  let screen = defaultScreenOfDisplay dpy
  elems <- reader stdin valuer
  rect  <- findRectangle dpy (rootWindowOfScreen screen)
  sel   <- gpick dpy screen rect defaultGPConfig elems
  case sel of
    Nothing -> exitFailure
    Just el -> putStrLn el >> exitSuccess
    where reader
           | cfg_complex cfg = readElementsC "stdin"
           | otherwise       = readElements
          valuer
           | cfg_enumerate cfg = \_ i -> show i
           | otherwise         = \ s _ -> s

setupDisplay :: String -> IO Display
setupDisplay dstr = do
  dpy <- openDisplay dstr `Prelude.catch` \_ -> do
    error $ "Cannot open display '" ++ dstr ++ "'."
  return dpy

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
                          , el_disp   = line 
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
                el_data = f (el_disp elm) num }
                      
parseElements :: SourceName -> String -> Either ParseError [Element a]
parseElements = parse $ many element <* eof

blankElem :: Element a
blankElem = Element {
              el_colors = ("black", "white")
            , el_data   = error "Element without data"
            , el_disp   = error "Element without display"
            , el_tags   = []
            }

element :: GenParser Char u (Element a)
element = do kvs <- kvPair `sepBy1` realSpaces <* spaces
             foldM procKv blankElem kvs
    where procKv elm ("disp", [val]) =
            return elm { el_disp = val }
          procKv _   ("disp", _) = badval "disp"
          procKv elm ("fg", [val]) =
            return elm {
              el_colors = (val, snd $ el_colors elm) }
          procKv _   ("fg", _) = badval "fg"
          procKv elm ("bg", [val]) =
            return elm {
              el_colors = (fst $ el_colors elm, val) }
          procKv _   ("bg", _) = badval "bg"
          procKv elm ("tags",val) =
            return elm { el_tags = el_tags elm ++ val }
          procKv _ (k, _) = nokey k
          badval k = parserFail $ "Bad value for field " ++ k
          nokey  k = parserFail $ "Unknown key " ++ k

kvPair :: GenParser Char u (String, [String])
kvPair = do
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
