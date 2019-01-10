{-# LANGUAGE RecordWildCards #-}
module GenPIO (genPIO) where

import Types
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M

type Pad = Int

genPIO :: Peripheral -> [String]
genPIO Peripheral{peripheralGroupName = Just "PIO",..} = makeStruct (initCaps peripheralName) body
    where body = concatMap (either padDecl registerDecl) $ padRegisters rs
          ([], rs) = partitionEithers peripheralRegisters
genPIO _ = []

makeStruct :: String -> [String] -> [String]
makeStruct name body = "#[repr(C)]" : strct : map ("    "++) body ++ [ "}", "" ]
    where strct = "pub struct " ++ name ++ " {"

registerDecl :: Register -> [String]
registerDecl Register{..} = ("// " ++ registerDescription) : ((:[]) . concat)
    [ "pub " ++ map toLower registerName
    , ": "
    , rw registerAccess
    , "<u32>"
    , ","
    , "\t// " ++ show registerAddressOffset
    , "\t" ++ show registerSize
    ]

padDecl :: Pad -> [String]
padDecl x = (:[]) $ concat
    [ "reserved_"
    , show x
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

padRegisters :: [Register] -> [Either Pad Register]
padRegisters rs = M.elems $ m `M.union` u
    where m = M.fromList [ (registerAddressOffset r, Right r) | r <- rs ]
          u = M.fromList [ (x, Left x) | x <- [ 0, 4..maximum $ M.keys m ] ]

