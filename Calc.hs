{-# LANGUAGE DeriveGeneric #-}

module Calc where

import Data.Aeson
import GHC.Generics

data Op = Plus | Minus | Multiply | Divide

instance ToJSON Op where
  toJSON Plus = toJSON "+"
  toJSON Minus = toJSON "-"
  toJSON Multiply = toJSON "*"
  toJSON Divide = toJSON "/"

data Calculation = Calculation {
  lOperand :: Int,
  rOperand :: Int,
  operator :: Op,
  result :: Float
} deriving (Generic)

instance ToJSON Calculation
