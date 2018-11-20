module Main (main) where

import qualified Web.Zulip.Lambdabot

main :: IO ()
main = Web.Zulip.Lambdabot.runServer
