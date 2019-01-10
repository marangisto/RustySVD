{-# LANGUAGE RecordWildCards #-}
module PeripheralDecl (peripheralDecl) where

import Types
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)

type Pad = Int

peripheralDecl :: Peripheral -> [String]
peripheralDecl Peripheral{..} = concat
    [ [ ("// " ++ peripheralDescription), "" ]
    , makeStruct (initCaps peripheralName) body
    , makePointer peripheralName peripheralBaseAddress
    ]
    where body = concatMap (either padDecl registerDecl) $ padRegisters rs
          ([], rs) = partitionEithers peripheralRegisters

makeStruct :: String -> [String] -> [String]
makeStruct name body = "#[repr(C)]" : strct : map ("    "++) body ++ [ "}", "" ]
    where strct = "pub struct " ++ name ++ " {"

makePointer :: String -> Int -> [String]
makePointer name base = concat
    [ "let "
    , lowerCase name
    , " = "
    , hex base
    , " as *const "
    , initCaps name
    , ";"
    ] : [ "" ]

registerDecl :: Register -> [String]
registerDecl Register{..} = (:[]) $ concat
    [ "pub " ++ map toLower registerName
    , ": "
    , rw registerAccess
    , "<u32>"
    , ","
    , "    "
    , "// [" ++ show registerAddressOffset ++  "] "
    , registerDescription
    ]

padDecl :: Pad -> [String]
padDecl x = (:[]) $ concat
    [ "reserved_"
    , hex x
    , ": "
    , "u32"
    , ","
    ]

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

padRegisters :: [Register] -> [Either Pad Register]
padRegisters rs = M.elems $ m `M.union` u
    where m = M.fromList [ (registerAddressOffset r, Right r) | r <- rs ]
          u = M.fromList [ (x, Left x) | x <- [ 0, 4..maximum $ M.keys m ] ]

