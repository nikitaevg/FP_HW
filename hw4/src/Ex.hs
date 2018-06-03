{-# LANGUAGE TemplateHaskell   #-}

module Ex where

import TH

newtype Example = Example Int deriving (Show)

genTextShow ''Example
