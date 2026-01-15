#!/usr/bin/env stack
-- stack script --resolver lts-21.0 --package aeson,http-conduit,bytestring,text,directory
-- nanocode - minimal claude code alternative (Haskell)
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson, Data.Aeson.Key, Network.HTTP.Simple, GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as L
import System.Environment, System.Process, System.IO, Control.Monad, Data.Maybe
import Data.List (isPrefixOf, tails)

key = lookupEnv "OPENROUTER_API_KEY" >>= \or -> if isJust or then return (fromJust or) else getEnv "ANTHROPIC_API_KEY"
model = do
  m <- lookupEnv "MODEL"
  case m of
    Just v -> return v
    Nothing -> do or <- lookupEnv "OPENROUTER_API_KEY"; return $ if isJust or then "anthropic/claude-opus-4-5" else "claude-opus-4-5"
apiUrl = do or <- lookupEnv "OPENROUTER_API_KEY"; return $ maybe "https://api.anthropic.com/v1/messages" (const "https://openrouter.ai/api/v1/messages") or
r = "\ESC[0m"; b = "\ESC[1m"; d = "\ESC[2m"; c = "\ESC[36m"; g = "\ESC[32m"; bl = "\ESC[34m"

data Block = Block { btype :: String, text :: Maybe String, name :: Maybe String, bid :: Maybe String, input :: Maybe Object } deriving Generic
instance FromJSON Block where parseJSON = withObject "Block" $ \v -> Block <$> v .: "type" <*> v .:? "text" <*> v .:? "name" <*> v .:? "id" <*> v .:? "input"

tool "read" i = case parseMaybe (.: "path") i of
  Just p -> do
    let off = fromMaybe 0 (parseMaybe (.: "offset") i)
    let lim = fromMaybe (maxBound :: Int) (parseMaybe (.: "limit") i)
    content <- readFile p
    return . unlines . take lim . drop off $ zipWith (\n l -> show n ++ "| " ++ l) [1..] (lines content)
  _ -> return "error"
tool "write" i = case (,) <$> parseMaybe (.: "path") i <*> parseMaybe (.: "content") i of Just (p,c) -> writeFile p c >> return "ok"; _ -> return "error"
tool "edit" i = case (,,) <$> parseMaybe (.: "path") i <*> parseMaybe (.: "old") i <*> parseMaybe (.: "new") i of
  Just (p,o,n) -> do
    txt <- readFile p
    let count = length $ filter (isPrefixOf o) (tails txt)
    if count == 0 then return "error: old_string not found"
    else if count > 1 && not (fromMaybe False (parseMaybe (.: "all") i)) then return $ "error: old_string appears " ++ show count ++ " times, use all=true"
    else do
      let breakOn pat s = go "" s where
            go acc [] = (reverse acc, "")
            go acc rest@(r:rs) | pat `isPrefixOf` rest = (reverse acc, rest)
                               | otherwise = go (r:acc) rs
          replaceFirst s old new = case breakOn old s of
            (a, b) | null b -> s
            (a, b) -> a ++ new ++ drop (length old) b
          replaceAll s old new
            | null s = ""
            | isPrefixOf old s = new ++ replaceAll (drop (length old) s) old new
            | otherwise = head s : replaceAll (tail s) old new
          updated = if fromMaybe False (parseMaybe (.: "all") i)
                    then replaceAll txt o n
                    else replaceFirst txt o n
      writeFile p updated >> return "ok"
  _ -> return "error"
tool "bash" i = case parseMaybe (.: "cmd") i of Just cmd -> readCreateProcess (shell cmd) ""; _ -> return "error"
tool "glob" i = case parseMaybe (.: "pat") i of Just pat -> readCreateProcess (shell $ "find . -name '" ++ pat ++ "' | head -50") ""; _ -> return "none"
tool "grep" i = case parseMaybe (.: "pat") i of Just pat -> readCreateProcess (shell $ "grep -rn '" ++ pat ++ "' . | head -50") ""; _ -> return "none"
tool _ _ = return "unknown"

schema = "[{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"offset\":{\"type\":\"integer\"},\"limit\":{\"type\":\"integer\"}},\"required\":[\"path\"]}}," ++
         "{\"name\":\"write\",\"description\":\"Write\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"}},\"required\":[\"path\",\"content\"]}}," ++
         "{\"name\":\"edit\",\"description\":\"Replace\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"old\":{\"type\":\"string\"},\"new\":{\"type\":\"string\"},\"all\":{\"type\":\"boolean\"}},\"required\":[\"path\",\"old\",\"new\"]}}," ++
         "{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}}," ++
         "{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}," ++
         "{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}]"

ask k m msgs = do
  let body = "{\"model\":\"" ++ m ++ "\",\"max_tokens\":4096,\"system\":\"Concise assistant\",\"messages\":" ++ L.unpack (encode msgs) ++ ",\"tools\":" ++ schema ++ "}"
  url <- apiUrl
  req <- parseRequest ("POST " ++ url)
  let req' = setRequestHeader "Content-Type" ["application/json"]
           $ setRequestHeader "anthropic-version" ["2023-06-01"]
           $ (if isJust (lookupEnv "OPENROUTER_API_KEY") then setRequestHeader "Authorization" ["Bearer " <> L.toStrict (L.pack k)] else setRequestHeader "x-api-key" [L.toStrict $ L.pack k])
           $ setRequestBodyLBS (L.pack body) req
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
