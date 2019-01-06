{-# LANGUAGE RecordWildCards #-}
module Main where

import CMSIS_SVD_1_3_3
import Text.XML.HaXml.Parse 
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.XML.HaXml.OneOfN
import Text.ParserCombinators.Poly.Plain
import Data.List (isPrefixOf)
import Control.Monad.Extra
import Control.Monad

main :: IO ()
main = do
    let fn = "SAM3X8E.svd"
    xml <- readFile fn
    let (Document _ _ root _) = xmlParse fn xml
    let (Right Device{..}) = fst $ runParser elementDevice [CElem root noPos]
    print device_schemaVersion -- :: Xsd.Decimal
    whenJust device_vendor $ error . unhandled "Maybe StringType"
    whenJust device_vendorID $ error . unhandled "Maybe IdentifierType"
    print device_name -- :: IdentifierType
    whenJust device_series $ error . unhandled "Maybe StringType"
    print device_version -- :: StringType
    print device_description -- :: StringType
    whenJust device_licenseText $ error . unhandled "Maybe StringType"
    whenJust device_cpu $ error . unhandled "Maybe CpuType"
    whenJust device_headerSystemFilename $ error . unhandled "Maybe IdentifierType"
    whenJust device_headerDefinitionsPrefix $ error . unhandled "Maybe IdentifierType"
    print device_addressUnitBits -- :: ScaledNonNegativeInteger
    print device_width -- :: ScaledNonNegativeInteger
    whenJust device_size $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust device_access $ error . unhandled "Maybe AccessType"
    whenJust device_protection $ error . unhandled "Maybe ProtectionStringType"
    whenJust device_resetValue $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust device_resetMask $ error . unhandled "Maybe ScaledNonNegativeInteger"
    let Peripherals ps = device_peripherals
    putStrLn $ show (length ps) ++ " peripherals"
    forM_ ps $ \PeripheralType{..} -> do
        print peripheralType_name -- :: DimableIdentifierType
        print peripheralType_version -- :: Maybe StringType
        print peripheralType_description -- :: Maybe StringType

    let xs = filter (("PIO" `isPrefixOf`) . simpleTypeText . unDimableIdentifierType . peripheralType_name) ps
    mapM_ gpioPeripheral xs
    whenJust device_vendorExtensions $ error . unhandled "Maybe VendorExtensions"

gpioPeripheral :: PeripheralType -> IO ()
gpioPeripheral PeripheralType{..} = do
    print peripheralType_name
    whenJust peripheralType_derivedFrom $ error . unhandled "Maybe DimableIdentifierType"
    whenJust peripheralType_dim $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust peripheralType_dimIncrement $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust peripheralType_dimIndex $ error . unhandled "Maybe DimIndexType"
    whenJust peripheralType_dimName $ error . unhandled "Maybe IdentifierType"
    whenJust peripheralType_dimArrayIndex $ error . unhandled "Maybe DimArrayIndexType"
    print peripheralType_version -- :: Maybe StringType
    print peripheralType_description -- :: Maybe StringType
    whenJust peripheralType_alternatePeripheral $ error . unhandled "Maybe DimableIdentifierType"
    print peripheralType_groupName -- :: Maybe Xs.Name
    print peripheralType_prependToName -- :: Maybe IdentifierType
    whenJust peripheralType_appendToName $ error . unhandled "Maybe IdentifierType"
    whenJust peripheralType_headerStructName $ error . unhandled "Maybe DimableIdentifierType"
    whenJust peripheralType_disableCondition $ error . unhandled "Maybe StringType"
    print peripheralType_baseAddress -- :: ScaledNonNegativeInteger
    whenJust peripheralType_size $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust peripheralType_access $ error . unhandled "Maybe AccessType"
    whenJust peripheralType_protection $ error . unhandled "Maybe ProtectionStringType"
    whenJust peripheralType_resetValue $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust peripheralType_resetMask $ error . unhandled "Maybe ScaledNonNegativeInteger"
    print peripheralType_addressBlock -- :: [AddressBlockType]
    print peripheralType_interrupt -- :: [InterruptType]
    let Just (RegistersType rs) =  peripheralType_registers -- :: Maybe RegistersType
    print $ length rs
    mapM_ regs rs

regs :: OneOf2 ClusterType RegisterType -> IO ()
regs (OneOf2 _) = error "ClusterType not implemented"
regs (TwoOf2 r) = register r

register :: RegisterType -> IO ()
register RegisterType{..} = do
    whenJust registerType_derivedFrom $ error . unhandled "Maybe DimableIdentifierType"
    whenJust registerType_dim $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dimIncrement $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dimIndex $ error . unhandled "Maybe DimIndexType"
    whenJust registerType_dimName $ error . unhandled "Maybe IdentifierType"
    whenJust registerType_dimArrayIndex $ error . unhandled "Maybe DimArrayIndexType"
    print registerType_name -- :: DimableIdentifierType
    whenJust registerType_displayName $ error . unhandled "Maybe StringType"
    print registerType_description -- :: Maybe StringType
    print registerType_choice8 -- :: (Maybe (OneOf2 (Maybe (IdentifierType)) (Maybe (DimableIdentifierType))))
    print registerType_addressOffset -- :: ScaledNonNegativeInteger
    print registerType_size -- :: Maybe ScaledNonNegativeInteger
    print registerType_access -- :: Maybe AccessType
    whenJust registerType_protection $ error . unhandled "Maybe ProtectionStringType"
    print registerType_resetValue -- :: Maybe ScaledNonNegativeInteger
    whenJust registerType_resetMask $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dataType $ error . unhandled "Maybe DataTypeType"
    whenJust registerType_modifiedWriteValues $ error . unhandled "Maybe ModifiedWriteValuesType"
    whenJust registerType_writeConstraint $ error . unhandled "Maybe WriteConstraintType"
    whenJust registerType_readAction $ error . unhandled "Maybe ReadActionType"
    let Just (FieldsType fs) = registerType_fields -- :: Maybe FieldsType
    mapM_ fld fs

fld :: FieldType -> IO ()
fld FieldType{..} = do
    print fieldType_name -- :: DimableIdentifierType
    whenJust fieldType_derivedFrom $ error . unhandled "Maybe DimableIdentifierType"
    whenJust fieldType_dim $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust fieldType_dimIncrement $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust fieldType_dimIndex $ error . unhandled "Maybe DimIndexType"
    whenJust fieldType_dimName $ error . unhandled "Maybe IdentifierType"
    whenJust fieldType_dimArrayIndex $ error . unhandled "Maybe DimArrayIndexType"

    print fieldType_description -- :: Maybe StringType
    print fieldType_choice7 -- :: OneOf3 (ScaledNonNegativeInteger,ScaledNonNegativeInteger) (ScaledNonNegativeInteger,(Maybe (ScaledNonNegativeInteger))) BitRangeType
          -- ^ Choice between:
          --   
          --   (1) Sequence of:
          --   
          --     * lsb
          --   
          --     * msb
          --   
          --   (2) Sequence of:
          --   
          --     * bitOffset
          --   
          --     * bitWidth
          --   
          --   (3) bitRange
    print fieldType_access -- :: Maybe AccessType
    whenJust fieldType_modifiedWriteValues $ error . unhandled "ModifiedWriteValuesType"
    whenJust fieldType_writeConstraint $ error . unhandled "Maybe WriteConstraintType"
    whenJust fieldType_readAction $ error . unhandled "Maybe ReadActionType"
    when (not $ null fieldType_enumeratedValues) $ error . unhandled "[EnumerationType]" $ fieldType_enumeratedValues

unhandled :: Show a => String -> a -> String
unhandled s x = "unhandled " ++ s ++ ": " ++ show x
