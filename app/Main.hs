{-# LANGUAGE RecordWildCards #-}
module Main where

import Text.XML.HaXml.Parse 
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.ParserCombinators.Poly.Plain
import Data.List (isPrefixOf)
import CMSIS_SVD_1_3_3
import Control.Monad

main :: IO ()
main = do
    let fn = "SAM3X8E.svd"
--    svd <- readFile fn
--    let x = xmlParse' fn svd  
--    print x
    xml <- readFile fn
    let (Document _ _ root _) = xmlParse fn xml
    let (Right Device{..}) = fst $ runParser elementDevice [CElem root noPos]
    print device_schemaVersion -- :: Xsd.Decimal
--    print device_vendor -- :: Maybe StringType
--    print device_vendorID -- :: Maybe IdentifierType
    print device_name -- :: IdentifierType
--    print device_series -- :: Maybe StringType
    print device_version -- :: StringType
    print device_description -- :: StringType
--    print device_licenseText -- :: Maybe StringType
--    print device_cpu -- :: Maybe CpuType
--    print device_headerSystemFilename -- :: Maybe IdentifierType
--    print device_headerDefinitionsPrefix -- :: Maybe IdentifierType
    print device_addressUnitBits -- :: ScaledNonNegativeInteger
    print device_width -- :: ScaledNonNegativeInteger
--    print device_size -- :: Maybe ScaledNonNegativeInteger
--    print device_access -- :: Maybe AccessType
--    print device_protection -- :: Maybe ProtectionStringType
--    print device_resetValue -- :: Maybe ScaledNonNegativeInteger
--    print device_resetMask -- :: Maybe ScaledNonNegativeInteger
--        , device_peripherals :: Peripherals
    let Peripherals ps = device_peripherals
    putStrLn $ show (length ps) ++ " peripherals"
    forM_ ps $ \PeripheralType{..} -> do
        print peripheralType_name -- :: DimableIdentifierType
        print peripheralType_version -- :: Maybe StringType
        print peripheralType_description -- :: Maybe StringType

    let xs = filter (("PIO" `isPrefixOf`) . simpleTypeText . unDimableIdentifierType . peripheralType_name) ps
    mapM_ gpioPeripheral xs
--    print device_vendorExtensions -- :: Maybe VendorExtensions
--
gpioPeripheral :: PeripheralType -> IO ()
gpioPeripheral PeripheralType{..} = do
    print peripheralType_name
--    print peripheralType_derivedFrom -- :: Maybe DimableIdentifierType
--    print peripheralType_dim -- :: Maybe ScaledNonNegativeInteger
--    print peripheralType_dimIncrement -- :: Maybe ScaledNonNegativeInteger
--    print peripheralType_dimIndex -- :: Maybe DimIndexType
--    print peripheralType_dimName -- :: Maybe IdentifierType
--    print peripheralType_dimArrayIndex -- :: Maybe DimArrayIndexType
    print peripheralType_version -- :: Maybe StringType
    print peripheralType_description -- :: Maybe StringType
--    print peripheralType_alternatePeripheral -- :: Maybe DimableIdentifierType
    print peripheralType_groupName -- :: Maybe Xs.Name
    print peripheralType_prependToName -- :: Maybe IdentifierType
--    print peripheralType_appendToName -- :: Maybe IdentifierType
--    print peripheralType_headerStructName -- :: Maybe DimableIdentifierType
--    print peripheralType_disableCondition -- :: Maybe StringType
    print peripheralType_baseAddress -- :: ScaledNonNegativeInteger
--    print peripheralType_size -- :: Maybe ScaledNonNegativeInteger
--    print peripheralType_access -- :: Maybe AccessType
--    print peripheralType_protection -- :: Maybe ProtectionStringType
--    print peripheralType_resetValue -- :: Maybe ScaledNonNegativeInteger
--    print peripheralType_resetMask -- :: Maybe ScaledNonNegativeInteger
    print peripheralType_addressBlock -- :: [AddressBlockType]
    print peripheralType_interrupt -- :: [InterruptType]
    let Just (RegistersType rs) =  peripheralType_registers -- :: Maybe RegistersType
    print $ length rs
