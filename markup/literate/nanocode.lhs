nanocode - Literate Haskell Edition
====================================

This is **literate programming** - the code IS the documentation.
Donald Knuth invented this in 1984 with WEB/CWEB.

In literate Haskell, only lines starting with `>` are code.
Everything else is documentation in Markdown/LaTeX.

Introduction
------------

nanocode is a minimal Claude Code alternative. This literate version
demonstrates how AI agents could be written as readable documents.

> -- nanocode - minimal claude code alternative (Literate Haskell)
> -- ghc nanocode.lhs && ./nanocode
> -- Or: runhaskell nanocode.lhs

Module Definition
-----------------

We begin with our module declaration and imports:

> module Main where
>
> import System.Environment (getEnv, lookupEnv)
> import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
> import Control.Monad (when, unless)
> import Data.Maybe (fromMaybe)

ANSI Color Codes
----------------

Terminal colors make the interface more readable:

> r, b, d, c, g, bl :: String
> r  = "\x1b[0m"   -- Reset
> b  = "\x1b[1m"   -- Bold
> d  = "\x1b[2m"   -- Dim
> c  = "\x1b[36m"  -- Cyan
> g  = "\x1b[32m"  -- Green
> bl = "\x1b[34m"  -- Blue

Message Type
------------

Messages in the conversation have a role and content:

> data Message = Message
>   { role    :: String
>   , content :: String
>   } deriving (Show)

Tool Definitions
----------------

Our AI agent can use these tools:

> data Tool = Read FilePath
>           | Write FilePath String
>           | Edit FilePath String String
>           | Bash String
>           | Glob String
>           | Grep String
>           deriving (Show)

Tool Execution
--------------

Each tool performs a specific operation:

> executeTool :: Tool -> IO String
> executeTool (Read path) = do
>   content <- readFile path
>   let numbered = zipWith (\n l -> show n ++ "| " ++ l) [1..] (lines content)
>   return $ unlines numbered
>
> executeTool (Write path content) = do
>   writeFile path content
>   return "ok"
>
> executeTool (Edit path old new) = do
>   content <- readFile path
>   if old `isInfixOf` content
>     then do
>       writeFile path (replace old new content)
>       return "ok"
>     else return "error: not found"
>   where
>     isInfixOf needle haystack = needle `elem` tails haystack
>     replace _ _ [] = []
>     replace o n s@(x:xs)
>       | o `isPrefixOf` s = n ++ replace o n (drop (length o) s)
>       | otherwise = x : replace o n xs
>     isPrefixOf [] _ = True
>     isPrefixOf _ [] = False
>     isPrefixOf (p:ps) (s:ss) = p == s && isPrefixOf ps ss
>     tails [] = [[]]
>     tails s@(_:xs) = s : tails xs
>
> executeTool (Bash cmd) = return "executed"  -- Simplified
> executeTool (Glob pat) = return "files..."  -- Simplified
> executeTool (Grep pat) = return "matches..." -- Simplified

Main Loop
---------

The REPL (Read-Eval-Print Loop) is the heart of our agent:

> mainLoop :: [Message] -> IO ()
> mainLoop messages = do
>   putStr $ b ++ bl ++ "❯" ++ r ++ " "
>   hFlush stdout
>   input <- getLine
>   let trimmed = strip input
>
>   case trimmed of
>     ""   -> mainLoop messages
>     "/q" -> putStrLn "Goodbye!"
>     "/c" -> do
>       putStrLn $ g ++ "⏺ Cleared" ++ r
>       mainLoop []
>     _    -> do
>       let newMessages = messages ++ [Message "user" trimmed]
>       -- API call would go here
>       putStrLn ""
>       putStrLn $ c ++ "⏺" ++ r ++ " Literate Haskell speaks!"
>       putStrLn $ d ++ "  Code as documentation, documentation as code." ++ r
>       putStrLn ""
>       mainLoop newMessages
>   where
>     strip = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

Entry Point
-----------

The program starts here:

> main :: IO ()
> main = do
>   hSetBuffering stdout NoBuffering
>   model <- fromMaybe "claude-sonnet-4-20250514" <$> lookupEnv "MODEL"
>   putStrLn $ b ++ "nanocode" ++ r ++ " | " ++ d ++ "Literate Haskell + " ++ model ++ r
>   putStrLn ""
>   mainLoop []

Why Literate Programming for AI?
--------------------------------

1. **Self-documenting agents** - The code explains itself
2. **Knowledge capture** - Design decisions are preserved
3. **AI training data** - Clean code-documentation pairs
4. **Maintainability** - Future developers understand why
5. **Verification** - Proofs alongside implementation

Donald Knuth said:
> "Let us change our traditional attitude to the construction of programs:
> Instead of imagining that our main task is to instruct a computer what to do,
> let us concentrate rather on explaining to human beings what we want a
> computer to do."

In the AI era, literate programming becomes even more valuable.
AI agents should explain their reasoning, just like this document
explains the code.
