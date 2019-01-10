{-# LANGUAGE RecordWildCards #-}
module GenPIO (genPIO) where

import Types
import Data.Char (toLower, toUpper)

genPIO :: Peripheral -> [String]
genPIO Peripheral{peripheralGroupName = Just "PIO",..} = makeStruct (initCaps peripheralName) body
    where body = [ "pub " ++ map toLower registerName ++ ": " ++ rw registerAccess ++ "<u32>"
                 | Right Register{..} <- peripheralRegisters
                 ]
genPIO _ = []

makeStruct :: String -> [String] -> [String]
makeStruct name body = strct : map ("    "++) body ++ [ "}", "" ]
    where strct = "pub struct " ++ name ++ " {"

rw :: Maybe AccessType -> String
rw (Just AccessType_Read'only) = "RO"
rw (Just AccessType_Write'only) = "WO"
rw (Just AccessType_Read'write) = "RW"
--rw (Just AccessType_WriteOnce)
--rw (Just AccessType_Read'writeOnce)
rw _ = error "no access type specified"

initCaps :: String -> String
initCaps (x:xs) = toUpper x : map toLower xs

