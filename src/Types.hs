{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Types where

data SiteInfo = SiteInfo { root        :: String
                         , protocol    :: String
                         , title       :: String
                         , desc        :: String
                         } deriving Show

data Profile = Profile { first_name :: String
               , last_name :: String
               , age :: Int
               , works :: String
               , photo :: String
               } deriving Show