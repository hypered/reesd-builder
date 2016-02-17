module Main (main) where

import System.Console.CmdArgs.Explicit

import Reesd.Commands.Build

------------------------------------------------------------------------------
main :: IO ()
main = processArgs buildModes >>= processCmd
