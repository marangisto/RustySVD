{-# LANGUAGE RecordWildCards #-}
module Types (module Types) where

import CMSIS_SVD_1_3_3 (AccessType(..))

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

type Cluster = ()

data Register = Register
    { registerName          :: String
    , registerDescription   :: String
    , registerAlternative   :: Maybe (Either String String) -- alternateGroup / alternateRegister
    , registerAddressOffset :: Int
    , registerSize          :: Maybe Int
    , registerAccess        :: Maybe AccessType
    , registerResetValue    :: Maybe Int
    , registerDimension     :: Maybe Dimension
    , registerFields        :: [Field]
    } deriving (Eq, Show)

data Field = Field
    { fieldName         :: String
    , fieldDescription  :: String
    , fieldPosition     :: Position
    , fieldAccess       :: Maybe AccessType
    } deriving (Eq, Show)

data Position
    = LsbMsb (Int, Int)
    | OffsetWidth (Int, Maybe Int)
    | BitRange String
    deriving (Eq, Show)

data AddressBlock = AddressBlock
    { addressBlockOffset        :: Int
    , addressBlockSize          :: Int
    , addressBlockUsage         :: String
    , addressBlockProtection    :: Maybe String
    } deriving (Eq, Show)

data Interrupt = Interrupt
    { interruptName         :: String
    , interruptDescription  :: String
    , interruptValue        :: Int
    } deriving (Eq, Show)

newtype Dimension = Dimension (Int, Int, String)   -- (dim, increment, index)
    deriving (Eq, Show)

