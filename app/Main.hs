{-# LANGUAGE RecordWildCards #-}
module Main where

import ParseSVD
import PeripheralDecl
import Data.Maybe

main :: IO ()
main = do
    dev <- parseSVD "SAM3X8E.svd"
    mapM_ putStrLn $ concatMap peripheralDecl $ devicePeripherals dev

printDevice :: Device' -> IO ()
printDevice Device'{..} = do
    putStrLn $ deviceName ++ " : " ++ deviceDescription ++ ", version " ++ deviceVersion
    putStrLn $ "addressUnitBits = " ++ show deviceAddressUnitBits
            ++ ", width = " ++ show deviceWidth
            ++ ", peripherals = " ++ show (length devicePeripherals)
    mapM_ printPeripheral devicePeripherals

printPeripheral :: Peripheral -> IO ()
printPeripheral Peripheral{..} = do
    putStrLn $ "\t" ++ peripheralName ++ " : " ++ peripheralDescription ++ ", version " ++ fromMaybe "?" peripheralVersion
    putStrLn $ "\t\tgroupName = " ++ fromMaybe "" peripheralGroupName
            ++ ", prependToName = " ++ fromMaybe "" peripheralPrependToName
            ++ ", baseAddress = " ++ show peripheralBaseAddress 
            ++ ", registers = " ++ show (length peripheralRegisters)
    putStrLn $ "\t\taddressBlock = " ++ show peripheralAddressBlock
    putStrLn $ "\t\tinterrupt = " ++ show peripheralInterrupt
    mapM_ (either printCluster printRegister) peripheralRegisters

printCluster :: Cluster -> IO ()
printCluster = error "Cluster not yet implemented"

printRegister :: Register -> IO ()
printRegister Register{..} = do
    putStrLn $ "\t\t\t" ++ registerName ++ " : " ++ registerDescription
    putStrLn $ "\t\t\t\taddressOffset = " ++ show registerAddressOffset
            ++ ", size = " ++ maybe "" show registerSize
            ++ ", access = " ++ maybe "" show registerAccess
            ++ ", resetValue = " ++ maybe "" show registerResetValue
            ++ ", dimension = " ++ maybe "" show registerDimension
            ++ ", fields = " ++ show (length registerFields)
