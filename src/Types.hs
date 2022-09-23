{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types ( module Types.Parameter
             , module Types.Response
             , module Types.Operation 
             , module Types.Tags
             ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.List as L
import Lib
import Text.Read as T
import Types.Parameter
import Types.Response
import Types.Operation
import Types.Tags

data Security = Read | ReadWrite 
  deriving (Show, Read)

mkSecurity :: T.Text -> Security
mkSecurity x = case T.toLower x of 
                 "readwrite" -> ReadWrite
                 _ -> Read

data RequestBody = RequestBody
  { requestBodyDescription :: T.Text
  , requestBodyContent :: T.Text
  }
  deriving (Show)
