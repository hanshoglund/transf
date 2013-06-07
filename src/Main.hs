
module Main where

import Text.Transf
import Data.Semigroup
import Data.Traversable (mapM)     
import Data.Typeable
import Control.Exception
import Control.Monad.Error hiding (mapM)
import Control.Monad.Plus hiding (mapM)
import Prelude hiding (readFile, writeFile)

main = do
    res <- runTransf $ do
        input  <- readFile "in.md"  
        output <- runTransformation (censorT <> printT <> evalT <> musicT) input
        writeFile "out.md" output
    case res of
        Left e -> putStrLn $ "Error: " ++ e
        Right _ -> return ()
