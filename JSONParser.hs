{-# LANGUAGE OverloadedStrings #-}

module JSONParser
where

import Vector2
import Control.Applicative
import Control.Monad
import Data.Aeson

data Input = Input
           {
            tycon :: String,
            posn  :: Vector2,
            dir   :: Float,
            mag   :: Float
           }

instance FromJSON Input where
    parseJSON (Object v) =
      Input <$> v .: "tycon"
            <*> v .: "posn"
            <*> v .: "dir"
            <*> v .: "mag"
      -- A non-Object value is of the wrong type, so use mzero to fail.
    parseJSON _          = mzero

instance ToJSON Input where
    toJSON (Input tycon posn dir mag) =
      object [ "tycon" .= tycon
             , "posn"  .= posn
             , "dir"   .= dir
             , "mag"   .= mag
             ]

instance FromJSON Vector2 where
    parseJSON (Object v) =
      Vector2 <$> v .: "x"
              <*> v .: "y"
    parseJSON _          = mzero

instance ToJSON Vector2 where
    toJSON (Vector2 x y) =
      object [ "x" .= x
             , "y" .= y
             ]
