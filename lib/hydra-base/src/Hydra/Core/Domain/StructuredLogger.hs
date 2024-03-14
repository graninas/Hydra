{-# LANGUAGE DeriveAnyClass #-}

module Hydra.Core.Domain.StructuredLogger where

import           Hydra.Prelude


type AttributeKey = Text
type AttributeValue = Text
type Attribute = (AttributeKey, AttributeValue)
type Attributes = [Attribute]

