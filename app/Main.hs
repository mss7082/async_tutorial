{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (when)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Say
import Control.Applicative ((<|>))

getURL :: Text -> IO Text
getURL url = do
  say $ "Downloading " <> url <> ": Starting"
  threadDelay (1 * 1000000)
  say $ "Downloading: " <> url <> " 1 second has passed"

  -- when (url == "url1") $ error $ "connection aborted for" <> show url
  threadDelay (1 * 1000000)
  say $ "Downloading: " <> url <> " 2 seconds has passed"
  threadDelay (1 * 1000000)
  say $ "Downloading: " <> url <> " 3 seconds has passed"
  threadDelay (1 * 1000000)
  say $ "Downloading: " <> url <> " Done"

  return $ "Contents of " <> url

getURLWithDuration :: Int -> Text -> IO Text
getURLWithDuration seconds url = do
  say $ "Downloading " <> url <> ": Starting"
  for_ [1 .. seconds] $ \s -> do
    threadDelay (1 * 1000000)
    say $ "Downloading: " <> url <> ": " <> T.pack (show s) <> " second has passed"
  say $ "Downloading: " <> url <> " Done"

  return $ "Contents of " <> url

main :: IO ()
main = do
  ------------------------------------------------------
  -- res1 <- getURL "url1"
  -- res2 <- getURL "url2"
  -- print res1
  -- print res2
  ------------------------------------------------------
  -- withAsync (getURL "url1") $ \a1 -> do
  --   withAsync (getURL "url2") $ \a2 -> do
  --     page1 <- wait a1
  --     page2 <- wait a2
  --     print page1
  --     print page2
  ------------------------------------------------------
  -- (page1, page2) <- concurrently (getURL "url1") (getURL "url2")
  -- print page1
  -- print page2
  ----------------------------------------------------------
  -- let urls = ["url1", "url2", "url3", "url4"]
  -- pages <- mapConcurrently getURL urls
  -- print pages
  ----------------------------------------------------------
  -- eResult <- race (getURLWithDuration 5 "url1") (getURLWithDuration 3 "url2")
  -- print eResult
  ----------------------------------------------------------
  (page1, page2, page3) <- runConcurrently $ 
    (,,) 
      <$> Concurrently (getURL "url1") 
      <*> (Concurrently (getURL "url2a") <|> Concurrently (getURL "url2b"))
      <*> Concurrently (getURL "url3")
  print page1
  print page2
  print page3
