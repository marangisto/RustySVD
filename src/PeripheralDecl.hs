{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PeripheralDecl (peripheralDecl) where

import Types
import Language.Rust.Parser
import Language.Rust.Syntax as S
import Language.Rust.Pretty
import Language.Rust.Data.Ident
import Language.Rust.Data.Position (Span(..), Position(..))
import Data.List (isSuffixOf)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import System.IO

type Pad = Int

peripheralDecl :: Peripheral -> [Item ()]
peripheralDecl p = [ peripheralStruct p ]

removeMe = map (\r@Register{..} -> r {registerName = filter (/='%') registerName})

peripheralStruct :: Peripheral -> Item ()
peripheralStruct Peripheral{..} = StructItem attributes PublicV (mkIdent peripheralName) variantData generics ()
    where -- variantData = StructD [ registerStructField "xyz" "RW", reservedStructField "reserved_00" ] ()
          variantData = StructD (map (either reservedStructField registerStructField) $ padRegisters $ removeMe rs) ()
          generics = Generics [] [] whereClause ()
          whereClause = WhereClause [] ()
          attributes = [ Attribute Outer (Path False [PathSegment "repr" Nothing ()] ()) (delimTree Paren $ IdentTok "C") ()
                       , SugaredDoc Outer False (' ' : peripheralDescription) ()
                       ]
          ([], rs) = partitionEithers peripheralRegisters

padRegisters :: [Register] -> [Either Pad Register]
padRegisters rs = M.elems $ m `M.union` u
    where m = M.fromList [ (registerAddressOffset r, Right r) | r <- rs ]
          u = M.fromList [ (x, Left x) | x <- [ 0, 4..maximum $ M.keys m ] ]

registerStructField Register{..} = StructField (Just $ mkIdent $ lowerCase registerName) PublicV fieldType attributes ()
    where fieldType = PathTy Nothing (Path False [PathSegment (mkIdent $ rw registerAccess) (Just (AngleBracketed [] [u32Type] [] ())) ()] ()) ()
          attributes = [ SugaredDoc Outer False (' ' : offset registerAddressOffset ++ registerDescription) () ]

reservedStructField x = StructField (Just $ mkIdent $ "reserved" ++ hex x) InheritedV u32Type attributes ()
    where attributes = []

u32Type = PathTy Nothing (Path False [PathSegment "u32" Nothing ()] ()) ()

tokenTree x = Tree (Token (Span NoPosition NoPosition) x)

delimTree d x = Tree (Delimited { S.span = Span NoPosition NoPosition, delim = d, tts = tokenTree x })

rw :: Maybe AccessType -> String
rw (Just AccessType_Read'only) = "RO"
rw (Just AccessType_Write'only) = "WO"
rw (Just AccessType_Read'write) = "RW"
--rw (Just AccessType_WriteOnce)
--rw (Just AccessType_Read'writeOnce)
rw _ = error "no access type specified"

initCaps :: String -> String
initCaps (x:xs) = toUpper x : map toLower xs

lowerCase :: String -> String
lowerCase = map toLower

hex :: Int -> String
hex x = "0x" ++ showHex x ""

offset :: Int -> String
offset x = "[" ++ show x ++ "]: "

test :: IO ()
test = do
    let src = parse' @(SourceFile Span) " \
    \ // General Purpose Backup Register\n\
    \\n\
    \#[repr(C)]\n\
    \pub struct Gpbr {\n\
        \pub gpbr: RW<u32>,    // [0] General Purpose Backup Register\n\
        \reserved_0xe0: u32,\n\
        \pub wpmr: RW<u32>,    // [228] Write Protect Mode Register\n\
    \}\n\
    \\n\
    \\n\
    \// let gpbr = 0x400e1a90 as *const Gpbr;\n"
    writeSourceFile stdout src
