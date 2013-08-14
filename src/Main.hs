
module Main where

import System.Console.GetOpt
import Data.Default
import Data.Maybe
import Data.Semigroup hiding (Option)
import Text.Transf
import Text.Transf.Process

main        = defaultMain' "transf" optDesc transf
transf opts = haskellT <> musicT' (getMusicOpts opts) <> musicHaskellT' (getMusicOpts opts) <> musicExtraT


data Opt
    = Format     String
    | Resolution Int
    | Resize     Int
getFormat :: Opt -> Maybe String
getFormat     (Format a)        = Just a
getFormat     _                 = Nothing
getResolution (Resolution a)    = Just a
getResolution _                 = Nothing
getResize     (Resize a)        = Just a
getResize     _                 = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

getMusicOpts :: [Opt] -> MusicOpts
getMusicOpts xs = MusicOpts {
        format     = fromMaybe (format def) $ firstJust $ fmap getFormat xs,
        resolution = fromMaybe (resolution def) $ firstJust $ fmap getResolution xs,
        resize     = fromMaybe (resize def) $ firstJust $ fmap getResize xs
    }

optDesc :: [OptDescr Opt]
optDesc = [
        Option [] ["format"] (ReqArg Format "")              "Music output format",
        Option [] ["format"] (ReqArg (Resolution . read) "") "Music resolution",
        Option [] ["format"] (ReqArg (Resize . read) "")     "Music resize factor"
    ]

