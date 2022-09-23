{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenApiParse where

import Types.OpenApiSpec
import Control.Lens
import Data.Aeson (encode)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Types
import qualified Data.Text as T
import Data.List as L
import Data.Char (toLower)
import qualified Data.HashSet.InsOrd as HashSet
import Data.OpenApi as O
import Data.OpenApi.Declare as O
import Data.OpenApi.Internal as O
import Data.OpenApi.Lens as O
import Data.OpenApi.Operation as O
import Data.Functor.Identity 
import Lib

-- | TODO: Currently not being used, still using the default schema
openApiSpecHeader :: [T.Text]
openApiSpecHeader = 
  [ "openapi: 3.0.0"
    ,"info:"
    ,"  version: 1.0.0"
    ,"  title: Mercury External Api"
    ,"  description: OpenApi specs for the external facing mercury API"
    ,"servers:"
    ,"  - url: http://localhost:3000/api/v1/"
    ,"components:"
    ,"  securitySchemes:"
    ,"    BasicAuth:"
    ,"      type: http"
    ,"      scheme: basic"
    ,"security:"
    ,"  - BasicAuth: []"
    ,"paths:"
  ]

trimText :: [T.Text] -> [T.Text]
trimText = 
  map (T.replace "///" "") .
  L.filter (`notElem` (["{-", "-}"]::[T.Text]))

-- | The main converter to build the DocBlock out of lines of text (the comments above the routes)
textToSpec :: [T.Text] -> OpenApiDocBlock
textToSpec docBlock = OpenApiDocBlock
  { openApiDocBlockPath = find "path: " 
  , openApiDocBlockOperation = mkOperation $ find "operation: "
  , openApiDocBlockOperationId = find "operationId: " 
  , openApiDocBlockTags = mkTags $ find "tags: " 
  , openApiDocBlockSummary = find "summary: " 
  , openApiDocBlockParameters = mkParameters $ find "parameters: "
  , openApiDocBlockResponses = mkResponses $ find "responses: "
  }
    where 
      docBlockTrimmed = trimText docBlock
      find key = findLine key docBlockTrimmed

-- | An extremely ineffecient way to pull a single line of text out of a DocBlock
findLine :: T.Text -> [T.Text] -> T.Text
findLine keyWord block = 
  T.strip $ T.replace keyWord "" $
    case filter (T.isInfixOf keyWord) block of
      [] -> ""
      [x] -> x
      (x:_) -> x

-- ==========================================================================================================
-- ==========================================================================================================

-- | Takes an OpenApi object, encodes it in JSON and then prints it to console
parseOpenApiExample :: OpenApi -> IO ()
parseOpenApiExample = putStrLn . read . show . encode 

printOpenApi = parseOpenApiExample buildOpenApiSpecJson

-- ==========================================================================================================
-- ==========================================================================================================

-- TODO: we need a way to get ALL routes' DocBlock section. Perhaps some TH magic?
retrieveDocBlocks :: [OpenApiDocBlock]
retrieveDocBlocks =
  fmap (textToSpec . trimText)
    [ getUserDetailsDocBlock
    ]

-- | Root object for OpenApi3 Json file. 
buildOpenApiSpecJson :: OpenApi
buildOpenApiSpecJson = 
  spec & components.schemas .~ defs
  where
    (defs, spec) = runDeclare buildOpenApiSpecJsonDefinitions mempty

-- | Builds all paths for OpenApi3 json output
buildOpenApiSpecJsonDefinitions :: Declare (Definitions O.Schema) OpenApi
buildOpenApiSpecJsonDefinitions = do
  -- let mPaths = (fmap mkPathFromDocBlock retrieveDocBlocks) 
  somePath <- mkPathFromDocBlock . textToSpec $ trimText getUserDetailsDocBlock
  return $ mempty & paths .~ [ somePath ]

--main :: IO ()
--main = do 
--  putStrLn "\r\n---2-trimText manual-------------------------------------------------------------"
--  let block = trimText getUserDocBlock
--  mapM_ print block
--  putStrLn "\r\n---3--------------------------------------------------------------"
--  print $ findLine "path:" block
--  print $ findLine "operation:" block
--  print $ findLine "operationId:" block
--  print $ findLine "summary:" block
--  print . mkTags $ findLine "tags:" block
--  print . mkResponses $ findLine "responses:" block
--  print . mkParameters $ findLine "parameters:" block
--  let spec = textToSpec block 
--  print spec
--  putStrLn "\r\n---4--------------------------------------------------------------"
--  let block2 = textToSpec $ trimText getUserDetailsDocBlock
--  print block2
--  printOpenApi
--
