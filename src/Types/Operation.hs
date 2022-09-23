{-# LANGUAGE OverloadedStrings #-}

module Types.Operation where

import Data.Text (Text, toLower)
import Data.Aeson.TH
import qualified Data.Char as C

data Operation = 
    GET
  | PUT
  | POST
  | DELETE
  deriving (Show, Read)
-- $(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map C.toLower} ''Operation)

mkOperation :: Text -> Operation
mkOperation x = case toLower x of 
                  "get" -> GET
                  "put" -> PUT
                  "post" -> POST
                  "delete" -> DELETE

