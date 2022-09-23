module Types.Tags where

import Data.Text 

mkTags :: Text -> [Text]
mkTags input = read (unpack input) :: [Text]
