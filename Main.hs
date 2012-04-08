module Main where

import Hdcpu16.Parser
import Hdcpu16.Types
import System.Environment
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Numeric


main = do
          args <- getArgs
          let fn = (args !! 0)
          ins <- parseins fn
          let bin = runPut $ put (Prog ins)
          B.putStr bin
          return () 
          
