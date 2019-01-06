{-# LANGUAGE RecordWildCards #-}
module Main where

import ParseSVD

main :: IO ()
main = parseSVD "SAM3X8E.svd" >>= print

