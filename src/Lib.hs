{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Text (Text, pack)

-- STORING EXAMPLE DOC BLOCKS FOR TESTING 
getUserDocBlock :: [Text]
getUserDocBlock = 
    [ "{-"
    , "/// path: /User/{userId}"
    , "/// operation: Get"
    , "/// operationId: getUserInfo"
    , "/// tags: [\"user\"]"
    , "/// summary: Get user by id"
    --, "/// paramters: [ (name: userId, in: path, description: the users id, required: true)]"
    , "/// parameters: [ (\"userId\", Path, \"the users id as string\", True), (\"SomeQueryParam\", Query, \"the users id as string\", False)]"
    , "/// responses: [(200, \"success\"), (404, \"Resource Not Found\")]"
    , "-}"
    ]

getUserDetailsDocBlock :: [Text]
getUserDetailsDocBlock = 
    [ "{-"
    , "/// path: /User/{userId}/Details"
    , "/// operation: Get"
    , "/// operationId: getUserInfo"
    , "/// tags: [\"user\"]"
    , "/// summary: Get user by id"
    --, "/// paramters: [ (name: userId, in: path, description: the users id, required: true)]"
    , "/// parameters: [ (\"userId\", Path, \"the users id as string\", True)]"
    , "/// responses: [(200, \"success\")]"
    , "-}"
    ]

data UserInfo = UserInfo { name :: Text , age :: Int }
