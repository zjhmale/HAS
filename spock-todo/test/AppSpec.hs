{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module AppSpec where

import           Web.Spock                  hiding (json)
import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Model                      (getAppCfg)
import qualified Network.Wai                as Wai
import qualified Network.Wai.Test           as Test hiding (request)
import           Route                      (spockApp)
import           Test.Hspec
import qualified Test.Hspec.Wai             as Test
import           Test.Hspec.Wai.JSON
import           Web.Spock.Internal.Cookies

getSessCookie :: Test.SResponse -> Maybe T.Text
getSessCookie resp =
    let headers = Test.simpleHeaders resp
    in lookup "todo" $
       maybe [] (parseCookies . snd) $
       find (\h -> fst h == "Set-Cookie") headers

spec :: Spec
spec = describe "Todo App" $
  Test.with testApp $ it "should response with 'welcome' and 200 status" $ do
    Test.get "/" `Test.shouldRespondWith` [json|{msg:"welcome"}|] { Test.matchStatus = 200 }
    Test.postHtmlForm "/login" [("username", "cleantha"), ("password", "cleantha"), ("type", "signup")] `Test.shouldRespondWith` [json|{ok:true, err:"1"}|] { Test.matchStatus = 200 }
    Test.postHtmlForm "/login" [("username", "cleantha"), ("password", "cleantha"), ("type", "login")] `Test.shouldRespondWith` [json|{ok:true, err:"success"}|] { Test.matchStatus = 200 }
    resp <- Test.postHtmlForm "/login" [("username", "cleantha"), ("password", "cleantha"), ("type", "login")]
    let sessCookie = fromMaybe "" $ getSessCookie resp
        headers = [ ("Cookie", TE.encodeUtf8 $ "todo=" <> sessCookie)
                  , ("Content-Type", "application/json")
                  ]
    liftIO $ print sessCookie
    Test.request "POST" "/posts" headers "{\"title\": \"post-title\",\"content\": \"post-content\"}" `Test.shouldRespondWith` [json|{ok:true, output:"New post #1 created."}|] { Test.matchStatus = 200 }

testApp :: IO Wai.Application
testApp = do
  appCfg <- getAppCfg
  spockAsApp $ spockApp appCfg
