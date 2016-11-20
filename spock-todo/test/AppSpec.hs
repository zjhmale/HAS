{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module AppSpec where

import Web.Spock hiding (json)
import Web.Spock.Config

import Model (getAppCfg, connectionInfo, User(..), Post(..), migrateAll)
import Route (spockApp)
import Test.Hspec
import           Control.Monad.Logger
import           Control.Monad.IO.Class  (liftIO)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test hiding (request)
import qualified Test.Hspec.Wai as Test
import           Test.Hspec.Wai.JSON
import           Database.Esqueleto
import           Database.Persist.MySQL
import           Database.Persist.TH

spec :: Spec
spec = describe "Todo App" $
  Test.with testApp $ it "should response with 'welcome' and 200 status" $ do
    Test.get "/" `Test.shouldRespondWith` [json|{msg:"welcome"}|] { Test.matchStatus = 200 }
    Test.postHtmlForm "/login" [("username", "cleantha"), ("password", "cleantha"), ("type", "signup")] `Test.shouldRespondWith` [json|{ok:true, err:"1"}|] { Test.matchStatus = 200 }
    Test.postHtmlForm "/login" [("username", "cleantha"), ("password", "cleantha"), ("type", "login")] `Test.shouldRespondWith` [json|{ok:true, err:"success"}|] { Test.matchStatus = 200 }
    Test.post "/posts" "title=\"t\",content=\"c\"" `Test.shouldRespondWith` [json|{ok:true, output:"New post #1 created."}|] { Test.matchStatus = 200 }

testApp :: IO Wai.Application
testApp = do
  appCfg <- getAppCfg
  spockAsApp $ spockApp appCfg
