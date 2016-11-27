{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module AppSpec where

import           Database.Persist.MySQL   (runSqlPool)
import           Config
import qualified Network.Wai                as Wai
import           Route                      (app)
import Model
import           Test.Hspec
import qualified Test.Hspec.Wai             as Test
import           Test.Hspec.Wai.JSON

spec :: Spec
spec = describe "Todo App" $
  Test.with (testApp Test) $ it "should create a new post" $ do
    Test.get "/" `Test.shouldRespondWith` [json|{msg:"welcome"}|] { Test.matchStatus = 200 }
    let headers = [("Content-Type", "application/json; charset=utf-8")]
    Test.request "POST" "/posts" headers "{\"title\": \"post-title\",\"content\": \"post-content\"}" `Test.shouldRespondWith` [json|{ok:true, output:1}|] { Test.matchStatus = 200 }

testApp :: Environment -> IO Wai.Application
testApp env = do
  pool <- makePool env
  let cfg = Config { getPool = pool
                   , getEnv = env
                   }
  runSqlPool doMigrations pool
  return $ app cfg
