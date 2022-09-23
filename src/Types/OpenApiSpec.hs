{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types.OpenApiSpec where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Types
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Char (toLower)
import qualified Data.HashSet.InsOrd as HashSet
import Data.OpenApi as O
import Data.OpenApi.Declare as O
import Data.OpenApi.Internal as O
import Data.OpenApi.Lens as O
import Data.OpenApi.Operation as O
import Data.Functor.Identity 

-- | The doc block contains the least amount of info needed to create a minimum Open Api Path object.
data OpenApiDocBlock = OpenApiDocBlock
  { openApiDocBlockPath :: T.Text
  , openApiDocBlockOperation :: Types.Operation
  , openApiDocBlockOperationId :: T.Text
  , openApiDocBlockTags :: [T.Text]
  , openApiDocBlockSummary :: T.Text
  , openApiDocBlockParameters :: [Parameter]
  , openApiDocBlockResponses :: [Types.Response]
  }
  deriving (Show)
-- $(deriveJSON defaultOptions{fieldLabelModifier = drop (T.length "openApiDocBlockPath"), constructorTagModifier = map toLower} ''OpenApiDocBlock)

-- | Converts an OpenApiDocBlock into an OpenApi PathItem
mkPathFromDocBlock :: OpenApiDocBlock -> O.DeclareT (O.Definitions O.Schema) Data.Functor.Identity.Identity ([Char], O.PathItem)
mkPathFromDocBlock OpenApiDocBlock {..} = do 
  -- Building schema is ciurrently :JACKED:
  responseSchema <- declareResponse "application/json" (Proxy :: Proxy String)
  --let responseSchema = ("OK" & _Inline.content.at "application/json" ?~ (mempty & schema ?~ Ref (Reference "User")))
  let path = T.unpack openApiDocBlockPath
  pure 
    (path, mempty 
      & case openApiDocBlockOperation of
          GET -> get
          PUT -> put
          POST -> post
          DELETE -> delete
      ?~ (mempty
          & parameters .~ mkParams openApiDocBlockParameters
          -- TODO: Response parsing is currently :JACKED:
          -- & at 200 ?~ (T.unpack $ getDescriptionForResponse openApiDocBlockResponses 200)
          & at 200 ?~ "OK"
          & at 404 ?~ "Not Found"
         ))

