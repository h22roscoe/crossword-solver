{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ConceptNet where

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String.Utils

type Id = String

data Related = Related {
                 name :: Id,
                 weight :: Float
               }
             deriving (Show, Generic)

instance FromJSON Related where
  parseJSON = withObject "related" $ \o -> do
     name <- o .: "@id"
     w    <- o .: "weight"
     return Related { name = name, weight = w }

instance ToJSON Related where
  toJSON (Related { name = name, weight = w }) =
    object [ "@id"    .= name
           , "weight" .= w
           ]

data Return = Return {
                related :: [Related]
              }
            deriving (Show, Generic)

instance FromJSON Return where
  parseJSON = withObject "return" $ \o -> do
     related <- o .: "related"
     return Return { related = related }

instance ToJSON Return where
  toJSON (Return { related = related }) =
    object [ "related" .= related ]

baseURL :: String
baseURL = "http://api.conceptnet.io/related"

getRelated :: Id -> IO (Maybe [Related])
getRelated name
  = do
      ret <- fmap decode $ simpleHttp $ baseURL ++ name ++ "?filter=/c/en"
      if isJust ret then
        return $ fmap related (fromJust ret)
      else
        return Nothing

fromId :: Id -> String
fromId name = replace "_" " " $ drop (length ("/c/en/" :: String)) name

toId :: String -> Id
toId name = "/c/en/" ++ (replace " " "_" name)

getWords :: [Related] -> [String]
getWords = map (fromId . name)
