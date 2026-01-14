#!/usr/bin/env stack
-- stack script --resolver lts-21.0 --package aeson,http-conduit,bytestring,text,directory
-- nanocode - minimal claude code alternative (Haskell)
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson, Data.Aeson.Key, Network.HTTP.Simple, GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T, qualified Data.Text.IO as TIO
import System.Environment, System.Process, System.IO, Control.Monad, Data.Maybe

key = getEnv "ANTHROPIC_API_KEY"
model = fromMaybe "claude-sonnet-4-20250514" <$> lookupEnv "MODEL"
r = "\ESC[0m"; b = "\ESC[1m"; d = "\ESC[2m"; c = "\ESC[36m"; g = "\ESC[32m"; bl = "\ESC[34m"

data Block = Block { btype :: String, text :: Maybe String, name :: Maybe String, bid :: Maybe String, input :: Maybe Object } deriving Generic
instance FromJSON Block where parseJSON = withObject "Block" $ \v -> Block <$> v .: "type" <*> v .:? "text" <*> v .:? "name" <*> v .:? "id" <*> v .:? "input"

tool "read" i = case parseMaybe (.: "path") i of Just p -> readFile p >>= return . unlines . zipWith (\n l -> show n ++ "| " ++ l) [1..] . lines; _ -> return "error"
tool "write" i = case (,) <$> parseMaybe (.: "path") i <*> parseMaybe (.: "content") i of Just (p,c) -> writeFile p c >> return "ok"; _ -> return "error"
tool "bash" i = case parseMaybe (.: "cmd") i of Just cmd -> readCreateProcess (shell cmd) ""; _ -> return "error"
tool "glob" i = case parseMaybe (.: "pat") i of Just pat -> readCreateProcess (shell $ "find . -name '" ++ pat ++ "' | head -50") ""; _ -> return "none"
tool "grep" i = case parseMaybe (.: "pat") i of Just pat -> readCreateProcess (shell $ "grep -rn '" ++ pat ++ "' . | head -50") ""; _ -> return "none"
tool _ _ = return "unknown"

schema = "[{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}}," ++
         "{\"name\":\"write\",\"description\":\"Write\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"}},\"required\":[\"path\",\"content\"]}}," ++
         "{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}}," ++
         "{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}," ++
         "{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}]"

ask k m msgs = do
  let body = "{\"model\":\"" ++ m ++ "\",\"max_tokens\":4096,\"system\":\"Concise assistant\",\"messages\":" ++ L.unpack (encode msgs) ++ ",\"tools\":" ++ schema ++ "}"
  req <- parseRequest "POST https://api.anthropic.com/v1/messages"
  let req' = setRequestHeader "Content-Type" ["application/json"] $ setRequestHeader "anthropic-version" ["2023-06-01"] $ setRequestHeader "x-api-key" [L.toStrict $ L.pack k] $ setRequestBodyLBS (L.pack body) req
  getResponseBody <$> httpLBS req'

main = do
  k <- key; m <- model
  putStrLn $ b ++ "nanocode" ++ r ++ " | " ++ d ++ m ++ r ++ "\n"
  hSetBuffering stdout NoBuffering
  loop k m []

loop k m msgs = do
  putStr $ b ++ bl ++ "❯" ++ r ++ " "
  input <- getLine
  case input of
    "/q" -> return ()
    "" -> loop k m msgs
    "/c" -> putStrLn (g ++ "⏺ Cleared" ++ r) >> loop k m []
    _ -> do
      let msgs' = msgs ++ [object ["role" .= ("user" :: String), "content" .= input]]
      process k m msgs'

process k m msgs = do
  resp <- ask k m msgs
  let Just content = decode resp >>= parseMaybe (.: "content") :: Maybe [Block]
  results <- forM content $ \blk -> case btype blk of
    "text" -> putStrLn ("\n" ++ c ++ "⏺" ++ r ++ " " ++ fromMaybe "" (text blk)) >> return Nothing
    "tool_use" -> do
      let n = fromMaybe "" (name blk)
      putStrLn $ "\n" ++ g ++ "⏺ " ++ n ++ r
      result <- tool n (fromMaybe mempty (input blk))
      putStrLn $ "  " ++ d ++ "⎿ " ++ head (lines result) ++ r
      return $ Just $ object ["type" .= ("tool_result" :: String), "tool_use_id" .= bid blk, "content" .= result]
    _ -> return Nothing
  let msgs' = msgs ++ [object ["role" .= ("assistant" :: String), "content" .= content]]
  case catMaybes results of
    [] -> putStrLn "" >> loop k m msgs'
    rs -> process k m (msgs' ++ [object ["role" .= ("user" :: String), "content" .= rs]])
