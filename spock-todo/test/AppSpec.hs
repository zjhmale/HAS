{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import Web.Spock
import Web.Spock.Config

import Model (getAppCfg)
import Route (spockApp)
import Test.Hspec
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test hiding (request)
import qualified Test.Hspec.Wai as Test
import           Data.Aeson (Value(..), object, (.=))

spec :: Spec
spec = describe "Todo App" $ do
  Test.with testApp $ it "should response with 'welcome' and 200 status" $
    Test.get "/" `Test.shouldRespondWith` "{\"msg\":\"welcome\"}" { Test.matchStatus = 200 }

testApp :: IO Wai.Application
testApp = do
  appCfg <- getAppCfg
  spockAsApp $ spockApp appCfg
