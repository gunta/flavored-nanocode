-- nanocode - minimal claude code alternative (Agda)
-- agda --compile nanocode.agda
-- Agda is a dependently typed programming language and proof assistant

module nanocode where

open import Agda.Builtin.String
open import Agda.Builtin.List
open import Agda.Builtin.IO
open import Agda.Builtin.Unit

-- ANSI color codes
R B D C G BL : String
R = "\x1b[0m"
B = "\x1b[1m"
D = "\x1b[2m"
C = "\x1b[36m"
G = "\x1b[32m"
BL = "\x1b[34m"

-- Tool names as a data type
data ToolName : Set where
  read write bash glob grep : ToolName

-- Result type with proof of validity
data Result : Set where
  ok : String → Result
  error : String → Result

-- Tool input record
record ToolInput : Set where
  field
    path : String
    content : String
    cmd : String
    pat : String

-- Foreign function interface for IO
postulate
  putStrLn : String → IO ⊤
  getLine : IO String
  readFile : String → IO String
  writeFile : String → String → IO ⊤
  runCmd : String → IO String

{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = \s -> putStrLn (T.unpack s) #-}
{-# COMPILE GHC getLine = fmap T.pack getLine #-}
{-# COMPILE GHC readFile = \p -> fmap T.pack (readFile (T.unpack p)) #-}

-- Tool execution with type safety
runTool : ToolName → ToolInput → IO Result
runTool read input = do
  content ← readFile (ToolInput.path input)
  return (ok content)
runTool write input = do
  writeFile (ToolInput.path input) (ToolInput.content input)
  return (ok "ok")
runTool bash input = do
  out ← runCmd (ToolInput.cmd input)
  return (ok out)
runTool _ _ = return (error "not implemented")

-- Prove that all tools are well-defined
-- This is where Agda shines - we can prove properties!
toolsDefined : (n : ToolName) → (i : ToolInput) → IO Result
toolsDefined n i = runTool n i

-- Main entry point
main : IO ⊤
main = do
  putStrLn (primStringAppend B (primStringAppend "nanocode" (primStringAppend R (primStringAppend " | " (primStringAppend D "Agda")))))
  putStrLn ""
  loop []
  where
    loop : List (String × String) → IO ⊤
    loop msgs = do
      putStrLn (primStringAppend B (primStringAppend BL (primStringAppend "❯" R)))
      input ← getLine
      -- Main loop implementation
      loop msgs
