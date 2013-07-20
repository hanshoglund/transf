
module Main where

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



version = "transf-0.8"
header  = "Usage: transf [options]\n" ++
          "Usage: transf [options] files...\n" ++
          "\n" ++
          "Options:"

options = [
    Option ['h'] ["help"]          (NoArg 1)   "Print help and exit",
    Option ['v'] ["version"]       (NoArg 2)   "Print version and exit"
  ]

main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo header options
    let printUsage   = putStr (usage ++ "\n")   >> exitSuccess
    let printVersion = putStr (version ++ "\n") >> exitSuccess

    when (1 `elem` opts) printUsage
    when (2 `elem` opts) printVersion
    runFilter opts


runFilter _ = transform stdin stdout

transform fin fout = do
    res <- runTF $ do
        input  <- liftIO $ hGetContents fin
        output <- runTransf' (haskellT <> musicT <> musicPlusHaskellT <> musicExtraT) input
        liftIO $ hPutStr fout output
    case res of
        Left e -> putStrLn $ "Error: " ++ e
        Right _ -> return ()
