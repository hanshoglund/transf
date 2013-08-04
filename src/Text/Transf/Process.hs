
module Text.Transf.Process (
        defaultMain,
  ) where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Plus hiding (mapM)
import Data.Semigroup hiding (Option)
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Traversable (mapM)
import Data.Typeable
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Text.Transf

import Prelude hiding (readFile, writeFile)

-- |
-- Creates a Unix style text processor from a 'Transform'.
--
-- The resulting action should be used as the main of an application
-- and will render a program of the given name that responds to @-v@
-- and @-h@ flags. If given no flags it runs the text transformer over
-- the standard input and output streams. If an error occurs the program
-- halts and prints an error message to the standard error stream.
-- 
defaultMain :: String -> Transform -> IO ()
defaultMain name transf = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo (header name) options
    let printUsage   = putStr (usage ++ "\n")        >> exitSuccess
    let printVersion = putStr (version name ++ "\n") >> exitSuccess

    when (1 `elem` opts) printUsage
    when (2 `elem` opts) printVersion
    runFilter transf opts

version name = name ++ "-0.8"
header  name = "Usage: "++name++" [options]\n" ++
               "Usage: "++name++" [options] files...\n" ++
               "\n" ++
               "Options:"

options = [
    Option ['h'] ["help"]          (NoArg 1)   "Print help and exit",
    Option ['v'] ["version"]       (NoArg 2)   "Print version and exit"
  ]

runFilter transf _ = run transf stdin stdout

run transf fin fout = do
    res <- runContext $ do
        input  <- liftIO $ hGetContents fin
        output <- runTransform' transf input
        liftIO $ hPutStr fout output
    case res of
        Left e  -> hPutStrLn stderr ("Error: " ++ e) >> exitFailure
        Right _ -> exitSuccess
