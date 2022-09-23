{-# LANGUAGE RecordWildCards #-}
module Types.Parameter where

import Data.Aeson
import Data.Aeson.TH
import Data.Text as T hiding (map)
import Data.List as L
import Text.Read as T 
import Control.Lens
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import Data.Char (toLower)
import qualified Data.HashSet.InsOrd as HashSet
import Data.OpenApi as O
import Data.OpenApi.Declare as O
import Data.OpenApi.Internal as O
import Data.OpenApi.Lens as O
import Data.OpenApi.Operation as O
import Data.Functor.Identity 

data Parameter = Parameter 
  { parameterName :: Text
  , parameterInLocation :: ParameterLocation
  , parameterDescription :: T.Text 
  , parameterRequired :: Bool 
  }
  deriving (Show)

mkParameter :: (T.Text, ParameterLocation, T.Text, Bool) -> Parameter
mkParameter (n, i, d, r) = 
  Parameter 
    { parameterName = n
    , parameterInLocation = i
    , parameterDescription = d
    , parameterRequired = r
    } 

-- "/// paramters: [ (name: userId, in: path, description: the users id, required: true)]"
mkParameters :: T.Text -> [Parameter]
mkParameters input = 
  map mkParameter (T.read (T.unpack input) :: [(T.Text, ParameterLocation, T.Text, Bool)])

data ParameterLocation = 
    Query | Header | Path | Cookie
  deriving (Show, Read)

-- | Convers DocBlock Params into OpenApi Params
mkParams :: [Parameter] -> [Referenced Param] 
mkParams = fmap mkOpenApiParamFromDocBlockParam

-- | Converts the DocBlock Parameter type into the OpenApi Parameter type
mkOpenApiParamFromDocBlockParam :: Parameter -> Referenced Param
mkOpenApiParamFromDocBlockParam Parameter {..} = 
  Inline $ mempty
     & name .~ parameterName 
     & required ?~ True
     & description ?~ parameterDescription 
     & in_ .~ mkParamIn parameterInLocation 
     -- TODO: Dynamic schema parsing is :JACKED: but as you can see, we do support Proxy a parsing, so if there is a good dynamic way to pass a type through the DocBlock object, we could implement this
     & schema ?~ Inline (toParamSchema (Proxy :: Proxy String))

-- | Converts the DocBlock level Param Location to the OpenApi spec Param Location.
-- Perhaps we could just use the OpenApi ParamLocation type instead when building DocBlock items?
mkParamIn :: ParameterLocation -> ParamLocation
mkParamIn location = case location of
  Types.Parameter.Path -> ParamPath
  Types.Parameter.Query -> ParamQuery
  Types.Parameter.Header -> ParamHeader
  Types.Parameter.Cookie -> ParamCookie

