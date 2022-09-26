{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Response where

import Data.Aeson
import Data.Aeson.TH
import Data.Text as T hiding (map)
import Data.List as L
import Lib
import Text.Read as T 

import Data.Char (toLower)
import qualified Data.HashSet.InsOrd as HashSet
import qualified Data.OpenApi as O
import qualified Data.OpenApi.Declare as O
import qualified Data.OpenApi.Internal as O
import qualified Data.OpenApi.Lens as O
import qualified Data.OpenApi.Operation as O
import qualified Data.Functor.Identity 

-- TODO: Currently the OpenApiSpec is hardcoding 200, OK and 404 Not Found. 
-- But, the DocBlocks already support the Response type. 
-- We would need to add Response parsing into mkPathFromDocBlock for the OpenApi json to generate 

data ResponseContent = 
    Array | Object
  deriving (Show)

data Response = Response
    { responseCode :: Int
    , responseDescription :: T.Text
    }
  deriving (Show)

mkResponse :: (Int, T.Text) -> Response 
mkResponse (i, d) = 
  Response 
    { responseCode = i
    , responseDescription = d
    }

mkResponses :: T.Text -> [Response]
mkResponses input = 
  map mkResponse (T.read (T.unpack input) :: [(Int, T.Text)])

-- | TODO: This was a WIP workaround to be able to easily parse DocBlock responses into OpenApi Response Descriptions
-- That said, it isnt effecient
getDescriptionForResponse :: [Response] -> Int -> T.Text -- O.Response
getDescriptionForResponse responses code = 
    case (L.filter (\x -> responseCode x == code) responses) of
      [] -> ""
      (Response {..}:_) -> responseDescription

