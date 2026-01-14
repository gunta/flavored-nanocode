-- nanocode - minimal claude code alternative (PureScript)
-- spago run
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.ChildProcess (execSync)
import Data.Maybe (fromMaybe)
import Data.Array (mapWithIndex, take)
import Data.String (split, joinWith, trim, Pattern(..))
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)

r = "\x1b[0m"
b = "\x1b[1m"
d = "\x1b[2m"
c = "\x1b[36m"
g = "\x1b[32m"
bl = "\x1b[34m"

tool :: String -> Json -> Effect String
tool "read" input = do
  let path = fromMaybe "" $ decodeJson input >>= \o -> o.path
  content <- readTextFile path
  pure $ joinWith "\n" $ mapWithIndex (\i l -> show (i + 1) <> "| " <> l) $ split (Pattern "\n") content
tool "write" input = do
  let path = fromMaybe "" $ decodeJson input >>= \o -> o.path
  let content = fromMaybe "" $ decodeJson input >>= \o -> o.content
  writeTextFile path content
  pure "ok"
tool "bash" input = do
  let cmd = fromMaybe "" $ decodeJson input >>= \o -> o.cmd
  result <- execSync cmd
  pure result
tool _ _ = pure "unknown"

schema :: Array Json
schema = 
  [ encodeJson { name: "read", description: "Read file", input_schema: { type: "object", properties: { path: { type: "string" } }, required: ["path"] } }
  , encodeJson { name: "write", description: "Write file", input_schema: { type: "object", properties: { path: { type: "string" }, content: { type: "string" } }, required: ["path", "content"] } }
  , encodeJson { name: "bash", description: "Run command", input_schema: { type: "object", properties: { cmd: { type: "string" } }, required: ["cmd"] } }
  ]

main :: Effect Unit
main = launchAff_ do
  key <- liftEffect $ lookupEnv "ANTHROPIC_API_KEY" <#> fromMaybe ""
  model <- liftEffect $ lookupEnv "MODEL" <#> fromMaybe "claude-sonnet-4-20250514"
  liftEffect $ log $ b <> "nanocode" <> r <> " | " <> d <> "PureScript + " <> model <> r <> "\n"
  -- Main REPL loop would continue here
  pure unit
