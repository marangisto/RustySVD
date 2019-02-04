{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import ParseSVD
import PeripheralDecl
import Language.Rust.Syntax
import Language.Rust.Pretty
import System.Console.CmdArgs
import Text.HTML.TagSoup
import Control.Monad
import Data.Maybe
import System.IO

data Options = Options
    { schema_version :: Bool
    , files :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { schema_version = def &= help "Show schema version"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate device descriptions from SVD files" &=
    summary "RustySVD v0.0.0, (c) Bengt Marten Agren 2018-2019" &=
    details [ "RustySVD generated device header files for ARM-based"
            , "MCUs based on vendor SVD files (see CMSIS)."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    mapM_ (process opts) files

process :: Options -> FilePath -> IO ()
process Options{schema_version=True,..} fn = putStrLn . ((fn++": ")++) =<< getSchemaVersion fn
process Options{..} fn = do
        dev <- parseSVD fn
        let src :: SourceFile ()
            src = SourceFile Nothing [] $ concat [ peripheralDecl p | p  <- devicePeripherals dev ]
        writeSourceFile stdout src
        putStrLn ""

getSchemaVersion :: FilePath -> IO String
getSchemaVersion fn = f . parseTags <$> readFile fn
    where f = fromAttrib "schemaVersion" . head . dropWhile (~/= ("<device>" :: String))

