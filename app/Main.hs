{-# LANGUAGE RecordWildCards #-}
module Main where

import CMSIS_SVD_1_3_3
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.XML.HaXml.OneOfN
import Text.ParserCombinators.Poly.Plain
import Data.List (isPrefixOf, intercalate)
import Control.Monad.Extra
import Control.Monad
import Data.Maybe

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
regs (TwoOf2 r) = register r >>= print

data Register = Register
    { registerName          :: String
    , registerDesc          :: String
    , registerAlternative   :: Maybe (Either String String) -- alternateGroup / alternateRegister
    , registerAddressOffset :: Int
    , registerSize          :: Maybe Int
    , registerAccess        :: Maybe AccessType
    , registerResetValue    :: Maybe Int
    , registerFields        :: [Field]
    } deriving (Eq, Show)

register :: RegisterType -> IO Register
register RegisterType{..} = do
    let registerName = dimableIdentifierToString registerType_name
        registerDesc = maybe "" stringTypeToString registerType_description
        registerAlternative = choice8 registerType_choice8
        registerAddressOffset = scaledNonNegativeIntegerToInt registerType_addressOffset
        registerSize = scaledNonNegativeIntegerToInt <$> registerType_size
        registerAccess = registerType_access
        registerResetValue = scaledNonNegativeIntegerToInt <$> registerType_resetValue
    let Just (FieldsType fs) = registerType_fields -- :: Maybe FieldsType
        registerFields = map fld fs
    whenJust registerType_derivedFrom $ error . unhandled "Maybe DimableIdentifierType"
    whenJust registerType_dim $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dimIncrement $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dimIndex $ error . unhandled "Maybe DimIndexType"
    whenJust registerType_dimName $ error . unhandled "Maybe IdentifierType"
    whenJust registerType_dimArrayIndex $ error . unhandled "Maybe DimArrayIndexType"
    whenJust registerType_displayName $ error . unhandled "Maybe StringType"
    whenJust registerType_protection $ error . unhandled "Maybe ProtectionStringType"
    whenJust registerType_resetMask $ error . unhandled "Maybe ScaledNonNegativeInteger"
    whenJust registerType_dataType $ error . unhandled "Maybe DataTypeType"
    whenJust registerType_modifiedWriteValues $ error . unhandled "Maybe ModifiedWriteValuesType"
    whenJust registerType_writeConstraint $ error . unhandled "Maybe WriteConstraintType"
    whenJust registerType_readAction $ error . unhandled "Maybe ReadActionType"
    return Register{..}

fld :: FieldType -> Field
fld FieldType{..} =
    let fieldName = dimableIdentifierToString fieldType_name
        fieldDesc = maybe "" stringTypeToString fieldType_description
        fieldPos =  choice7 fieldType_choice7
        fieldAccess = fieldType_access
    in seq (errorIfNotNull xs) Field{..}
    where xs = [ unhandled "fieldType_derivedFrom" <$> fieldType_derivedFrom
               , unhandled "fieldType_dim" <$> fieldType_dim
               , unhandled "fieldType_dimIncrement" <$> fieldType_dimIncrement
               , unhandled "fieldType_dimIndex" <$> fieldType_dimIndex
               , unhandled "fieldType_dimName" <$> fieldType_dimName
               , unhandled "fieldType_dimArrayIndex" <$> fieldType_dimArrayIndex
               , unhandled "fieldType_modifiedWriteValues" <$> fieldType_modifiedWriteValues
               , unhandled "fieldType_writeConstraint" <$> fieldType_writeConstraint
               , unhandled "fieldType_readAction" <$> fieldType_readAction
               , unhandled "fieldType_enumeratedValues" <$> listToMaybe fieldType_enumeratedValues
               ]

data Field = Field
    { fieldName     :: String
    , fieldDesc     :: String
    , fieldPos      :: Choice7
    , fieldAccess   :: Maybe AccessType
    } deriving (Eq, Show)

data Choice7
    = LsbMsb (Int, Int)
    | OffsetWidth (Int, Maybe Int)
    | BitRange String
    deriving (Eq, Show)

choice7 :: OneOf3 (ScaledNonNegativeInteger,ScaledNonNegativeInteger)
                  (ScaledNonNegativeInteger,(Maybe (ScaledNonNegativeInteger)))
                  BitRangeType
        -> Choice7
choice7 (OneOf3 (x, y)) = LsbMsb (scaledNonNegativeIntegerToInt x, scaledNonNegativeIntegerToInt y)
choice7 (TwoOf3 (x, y)) = OffsetWidth (scaledNonNegativeIntegerToInt x, scaledNonNegativeIntegerToInt <$> y)
choice7 (ThreeOf3 t) = BitRange $ read $ show t

choice8 :: Maybe (OneOf2 (Maybe IdentifierType) (Maybe DimableIdentifierType)) -> Maybe (Either String String)
choice8 (Just (OneOf2 (Just (IdentifierType x)))) = Just $ Left $ simpleTypeText x
choice8 (Just (TwoOf2 (Just (DimableIdentifierType x)))) = Just $ Right $ simpleTypeText x
choice8 _ = Nothing

scaledNonNegativeIntegerToInt = read . simpleTypeText . unScaledNonNegativeInteger

dimableIdentifierToString = simpleTypeText . unDimableIdentifierType

stringTypeToString = simpleTypeText . unStringType

unhandled :: Show a => String -> a -> String
unhandled s x = "unhandled " ++ s ++ ": " ++ show x

errorIfNotNull xs = let ys = catMaybes xs in if null ys then () else error $ intercalate "; " ys

