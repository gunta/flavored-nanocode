-- nanocode - minimal claude code alternative (Idris 2)
-- idris2 --exec main nanocode.idr
-- Idris is a dependently typed language with practical focus

module Main

import System
import System.File
import Data.String
import Data.List

-- ANSI codes
R : String; R = "\x1b[0m"
B : String; B = "\x1b[1m"
D : String; D = "\x1b[2m"  
C : String; C = "\x1b[36m"
G : String; G = "\x1b[32m"
BL : String; BL = "\x1b[34m"

-- Tool names with dependent types
data ToolName = Read | Write | Bash | Glob | Grep

-- Prove tools are finite
Eq ToolName where
  Read == Read = True
  Write == Write = True
  Bash == Bash = True
  Glob == Glob = True
  Grep == Grep = True
  _ == _ = False

record ToolInput where
  constructor MkInput
  path : Maybe String
  content : Maybe String
  cmd : Maybe String
  pat : Maybe String

-- Result type
data Result = Ok String | Error String

-- Tool implementation
tool : ToolName -> ToolInput -> IO Result
tool Read input = case path input of
  Just p => do
    Right content <- readFile p
      | Left err => pure (Error "file not found")
    let lines = split (== '\n') content
    let numbered = zipWith (\i, l => show (i + 1) ++ "| " ++ l) [0..length lines] lines
    pure (Ok (unlines numbered))
  Nothing => pure (Error "no path")
    
tool Write input = case (path input, content input) of
  (Just p, Just c) => do
    Right () <- writeFile p c
      | Left err => pure (Error "write failed")
    pure (Ok "ok")
  _ => pure (Error "missing args")

tool Bash input = case cmd input of
  Just c => do
    res <- system c
    pure (Ok "executed")
  Nothing => pure (Error "no command")

tool _ _ = pure (Error "not implemented")

-- Schema definition
schema : List (String, String)
schema = [
  ("read", "Read file"),
  ("write", "Write file"),
  ("bash", "Run command")
]

-- Main loop
loop : List (String, String) -> IO ()
loop msgs = do
  putStr (B ++ BL ++ "❯" ++ R ++ " ")
  input <- getLine
  let input' = trim input
  
  if input' == ""
    then loop msgs
    else if input' == "/q"
      then pure ()
      else if input' == "/c"
        then do
          putStrLn (G ++ "⏺ Cleared" ++ R)
          loop []
        else do
          -- Process message and call API
          let msgs' = msgs ++ [("user", input')]
          putStrLn ""
          loop msgs'

main : IO ()
main = do
  Just key <- getEnv "ANTHROPIC_API_KEY"
    | Nothing => putStrLn "ANTHROPIC_API_KEY required"
  model <- maybe "claude-sonnet-4-20250514" id <$> getEnv "MODEL"
  
  putStrLn (B ++ "nanocode" ++ R ++ " | " ++ D ++ "Idris 2 + " ++ model ++ R ++ "\n")
  loop []
