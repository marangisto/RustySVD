{-# LANGUAGE RecordWildCards #-}
module ParseSVD (parseSVD) where

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

parseSVD :: FilePath -> IO Device'
parseSVD fn = parseDevice fn <$> readFile fn

parseDevice :: String -> String -> Device'
parseDevice fn xml = seq (errorIfNotNull xs) Device'{..}
    where (Document _ _ root _) = xmlParse fn xml
          (Right Device{..}) = fst $ runParser elementDevice [CElem root noPos]
          deviceSchemaVersion = 0 -- FIXME: read $ simpleTypeText device_schemaVersion
          deviceName = simpleTypeText device_name
          deviceVersion = simpleTypeText device_version
          deviceDescription = stringTypeToString device_description
          deviceAddressUnitBits = scaledNonNegativeIntegerToInt device_addressUnitBits
          deviceWidth = scaledNonNegativeIntegerToInt device_width
          devicePeripherals = map gpioPeripheral $ filter (peripheralPrefix "PIO") $ unPeripherals device_peripherals
          xs = [ unhandled "device_vendor" <$> device_vendor 
               , unhandled "device_vendorID" <$> device_vendorID 
               , unhandled "device_series" <$> device_series 
               , unhandled "device_licenseText" <$> device_licenseText 
               , unhandled "device_cpu" <$> device_cpu 
               , unhandled "device_headerSystemFilename" <$> device_headerSystemFilename 
               , unhandled "device_headerDefinitionsPrefix" <$> device_headerDefinitionsPrefix 
               , unhandled "device_size" <$> device_size 
               , unhandled "device_access" <$> device_access 
               , unhandled "device_protection" <$> device_protection 
               , unhandled "device_resetValue" <$> device_resetValue 
               , unhandled "device_resetMask" <$> device_resetMask 
               , unhandled "device_vendorExtensions" <$> device_vendorExtensions 
               ]

data Device' = Device'
    { deviceSchemaVersion   :: Int
    , deviceName            :: String
    , deviceVersion         :: String
    , deviceDescription     :: String
    , deviceAddressUnitBits :: Int
    , deviceWidth           :: Int
    , devicePeripherals     :: [Peripheral]
    } deriving (Eq, Show)

data Peripheral = Peripheral
    { peripheralName            :: String
    , peripheralDescription     :: String
    , peripheralVersion         :: Maybe String
    , peripheralGroupName       :: Maybe String
    , peripheralPrependToName   :: Maybe String
    , peripheralBaseAddress     :: Int
    , peripheralAddressBlock    :: [AddressBlock]
    , peripheralInterrupt       :: [Interrupt]
    , peripheralRegisters       :: [Either Cluster Register]
    } deriving (Eq, Show)

gpioPeripheral :: PeripheralType -> Peripheral
gpioPeripheral PeripheralType{..} =
    let peripheralName = dimableIdentifierToString peripheralType_name
        peripheralDescription = maybe "" stringTypeToString peripheralType_description
        peripheralVersion = stringTypeToString <$> peripheralType_version
        peripheralGroupName = nameToString <$> peripheralType_groupName
        peripheralPrependToName = identifierToString <$> peripheralType_prependToName
        peripheralBaseAddress = scaledNonNegativeIntegerToInt peripheralType_baseAddress
        peripheralAddressBlock = map addressBlock peripheralType_addressBlock
        peripheralInterrupt = map interrupt peripheralType_interrupt
        Just (RegistersType rs) = peripheralType_registers
        peripheralRegisters = map clusterRegister rs
    in seq (errorIfNotNull xs) Peripheral{..}
    where xs = [ unhandled "peripheralType_derivedFrom" <$> peripheralType_derivedFrom
               , unhandled "peripheralType_dim" <$> peripheralType_dim
               , unhandled "peripheralType_dimIncrement" <$> peripheralType_dimIncrement
               , unhandled "peripheralType_dimIndex" <$> peripheralType_dimIndex
               , unhandled "peripheralType_dimName" <$> peripheralType_dimName
               , unhandled "peripheralType_dimArrayIndex" <$> peripheralType_dimArrayIndex
               , unhandled "peripheralType_alternatePeripheral" <$> peripheralType_alternatePeripheral
               , unhandled "peripheralType_appendToName" <$> peripheralType_appendToName
               , unhandled "peripheralType_headerStructName" <$> peripheralType_headerStructName
               , unhandled "peripheralType_disableCondition" <$> peripheralType_disableCondition
               , unhandled "peripheralType_size" <$> peripheralType_size
               , unhandled "peripheralType_access" <$> peripheralType_access
               , unhandled "peripheralType_protection" <$> peripheralType_protection
               , unhandled "peripheralType_resetValue" <$> peripheralType_resetValue
               , unhandled "peripheralType_resetMask" <$> peripheralType_resetMask
               ]

type Cluster = ()

clusterRegister :: OneOf2 ClusterType RegisterType -> Either Cluster Register
clusterRegister (OneOf2 _) = error "ClusterType not implemented"
clusterRegister (TwoOf2 r) = Right $ register r

data Register = Register
    { registerName          :: String
    , registerDescription   :: String
    , registerAlternative   :: Maybe (Either String String) -- alternateGroup / alternateRegister
    , registerAddressOffset :: Int
    , registerSize          :: Maybe Int
    , registerAccess        :: Maybe AccessType
    , registerResetValue    :: Maybe Int
    , registerFields        :: [Field]
    } deriving (Eq, Show)

register :: RegisterType -> Register
register RegisterType{..} =
    let registerName = dimableIdentifierToString registerType_name
        registerDescription = maybe "" stringTypeToString registerType_description
        registerAlternative = choice8 registerType_choice8
        registerAddressOffset = scaledNonNegativeIntegerToInt registerType_addressOffset
        registerSize = scaledNonNegativeIntegerToInt <$> registerType_size
        registerAccess = registerType_access
        registerResetValue = scaledNonNegativeIntegerToInt <$> registerType_resetValue
        Just (FieldsType fs) = registerType_fields -- :: Maybe FieldsType
        registerFields = map fld fs
    in seq (errorIfNotNull xs) Register{..}
    where xs = [ unhandled "registerType_derivedFrom" <$> registerType_derivedFrom
               , unhandled "registerType_dim" <$> registerType_dim
               , unhandled "registerType_dimIncrement" <$> registerType_dimIncrement
               , unhandled "registerType_dimIndex" <$> registerType_dimIndex
               , unhandled "registerType_dimName" <$> registerType_dimName
               , unhandled "registerType_dimArrayIndex" <$> registerType_dimArrayIndex
               , unhandled "registerType_displayName" <$> registerType_displayName
               , unhandled "registerType_protection" <$> registerType_protection
               , unhandled "registerType_resetMask" <$> registerType_resetMask
               , unhandled "registerType_dataType" <$> registerType_dataType
               , unhandled "registerType_modifiedWriteValues" <$> registerType_modifiedWriteValues
               , unhandled "registerType_writeConstraint" <$> registerType_writeConstraint
               , unhandled "registerType_readAction" <$> registerType_readAction
               ]

fld :: FieldType -> Field
fld FieldType{..} =
    let fieldName = dimableIdentifierToString fieldType_name
        fieldDescription = maybe "" stringTypeToString fieldType_description
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
    { fieldName         :: String
    , fieldDescription  :: String
    , fieldPos          :: Choice7
    , fieldAccess       :: Maybe AccessType
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

data AddressBlock = AddressBlock
    { addressBlockOffset        :: Int
    , addressBlockSize          :: Int
    , addressBlockUsage         :: String
    , addressBlockProtection    :: Maybe String
    } deriving (Eq, Show)

addressBlock :: AddressBlockType -> AddressBlock
addressBlock AddressBlockType{..} =
    let addressBlockOffset = scaledNonNegativeIntegerToInt addressBlockType_offset
        addressBlockSize = scaledNonNegativeIntegerToInt addressBlockType_size
        addressBlockUsage = simpleTypeText addressBlockType_usage
        addressBlockProtection = simpleTypeText <$> addressBlockType_protection
    in AddressBlock{..}

data Interrupt = Interrupt
    { interruptName         :: String
    , interruptDescription  :: String
    , interruptValue        :: Int
    } deriving (Eq, Show)

interrupt :: InterruptType -> Interrupt
interrupt InterruptType{..} =
    let interruptName = stringTypeToString interruptType_name
        interruptDescription = maybe "" simpleTypeText interruptType_description
        interruptValue = fromInteger interruptType_value
    in Interrupt{..}

scaledNonNegativeIntegerToInt = read . simpleTypeText . unScaledNonNegativeInteger

identifierToString = simpleTypeText . unIdentifierType

dimableIdentifierToString = simpleTypeText . unDimableIdentifierType

stringTypeToString = simpleTypeText . unStringType

nameToString (Name s) = s

unhandled :: Show a => String -> a -> String
unhandled s x = "unhandled " ++ s ++ ": " ++ show x

errorIfNotNull xs = let ys = catMaybes xs in if null ys then () else error $ intercalate "; " ys

peripheralPrefix :: String -> PeripheralType -> Bool
peripheralPrefix p = (p `isPrefixOf`) . simpleTypeText . unDimableIdentifierType . peripheralType_name

