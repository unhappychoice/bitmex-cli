module Main where

import Lib
import Console.Display
import Console.Options
import Data.Version

main :: IO ()
main = defaultMain $ do
  programName "bitmex-cli"
  programVersion $ makeVersion [0, 1, 0]
  programDescription "CLI tool for BitMEX"
  command "positions" $ do
    description "Displays a current positions"
    -- apiKeyParam <- flagParam (FlagShort 'k' <> FlagLong "api-key") keyFlagParser
    action $ \toParam -> getInstruments -- apiKeyParam

keyFlagParser :: FlagParser String
keyFlagParser = FlagOptional "h" $ \key -> Right key
