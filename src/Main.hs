
module Main where

import Data.Semigroup
import Text.Transf
import Text.Transf.Process

main   = defaultMain "transf" transf
transf = haskellT <> musicT <> musicHaskellT <> musicExtraT


