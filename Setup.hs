#!/usr/bin/env runhaskell

import Distribution.Simple.Setup (CopyDest(..),ConfigFlags(..),BuildFlags(..),
                                  CopyFlags(..),RegisterFlags(..),InstallFlags(..),
                                  defaultRegisterFlags,fromFlagOrDefault,Flag(..),
                                  defaultCopyFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
                            (LocalBuildInfo(..),absoluteInstallDirs)
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.InstallDirs
                            (InstallDirs(..))
import Distribution.Simple.Program 
                            (Program(..),ConfiguredProgram(..),ProgramConfiguration(..),
                             ProgramLocation(..),simpleProgram,lookupProgram,
                             rawSystemProgramConf)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Data.Char (isSpace, showLitChar)
import Data.List (isSuffixOf,isPrefixOf)
import Data.Maybe (listToMaybe,isJust)
import Data.Version
import Control.Monad (when,unless)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit
import System.IO (hGetContents,hClose,hPutStr,stderr)
import System.IO.Error (try)
import System.Process (runInteractiveProcess,waitForProcess)
import System.Directory
import System.Info (os)
import System.FilePath

main = defaultMainWithHooks gsmenuHooks
gsmenuHooks = simpleUserHooks { postInst = gsmenuPostInst
                              , postCopy = gsmenuPostCopy }

gsmenu = "gsmenu"

isWindows :: Bool
isWindows = os == "mingw" -- XXX

gsmenuPostInst a (InstallFlags { installPackageDB = db, installVerbosity = v }) pd lbi =
    do  gsmenuPostCopy a (defaultCopyFlags { copyDest = Flag NoCopyDest, copyVerbosity = v }) pd lbi

gsmenuPostCopy a (CopyFlags { copyDest = cdf, copyVerbosity = vf }) pd lbi =
    do let v         = fromFlagOrDefault normal vf
           cd        = fromFlagOrDefault NoCopyDest cdf
           manDir    = mandir $ (absoluteInstallDirs pd lbi cd)
           gsmenuDir = buildDir lbi `combine` gsmenu
       when (not isWindows) $ do
         putStrLn $ "Installing manpage in in " ++ manDir
         createDirectoryIfMissing True manDir
         copyFileVerbose v ("gsmenu.1") (manDir `combine` "gsmenu.1")
