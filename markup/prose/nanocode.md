# nanocode - Prose Edition

> A minimal Claude Code alternative written in natural language.
> See: https://www.prose.md/

## Configuration

Set the API key from the environment variable `ANTHROPIC_API_KEY`.
Set the model to "claude-sonnet-4-20250514" by default.
Initialize an empty message history.

## Main Program

Print "🧬 nanocode | Prose Language" followed by a blank line.

Repeat forever:
  - Print the prompt "❯ " without a newline.
  - Read a line of input from the user.
  - Trim whitespace from the input.
  
  - If the input is empty, continue to the next iteration.
  
  - If the input equals "/q":
    - Print "Goodbye!"
    - Exit the program.
  
  - If the input equals "/c":
    - Clear the message history.
    - Print "⏺ Cleared" in green.
    - Continue to the next iteration.
  
  - Append a user message to the history with the input as content.
  
  - Call the Agent Loop.
  
  - Print a blank line.

## Agent Loop

Repeat until no tool calls:
  - Call the Claude API with:
    - The configured model
    - Maximum 4096 tokens
    - System prompt: "Concise coding assistant"
    - The message history
    - The tool schema
  
  - For each block in the response content:
    - If the block type is "text":
      - Print "⏺ " in cyan followed by the text.
    
    - If the block type is "tool_use":
      - Print "⏺ " in green followed by the tool name.
      - Execute the tool with the given input.
      - Print "  ⎿ " in dim followed by the first line of the result.
      - Add a tool result to the results list.
  
  - Append an assistant message to the history.
  - If no tool results, break the loop.
  - Append a user message with the tool results.

## Tool Definitions

### Read Tool
Description: "Read file contents with line numbers"
Required: path (string)
Optional: offset (integer), limit (integer)

To execute:
  - Open the file at the given path.
  - Read all lines.
  - Number each line starting from offset + 1.
  - Return the numbered lines joined by newlines.

### Write Tool
Description: "Write content to a file"
Required: path (string), content (string)

To execute:
  - Open the file at the given path for writing.
  - Write the content.
  - Return "ok".

### Edit Tool
Description: "Replace text in a file"
Required: path (string), old (string), new (string)
Optional: all (boolean)

To execute:
  - Read the file content.
  - If old string not found, return "error: not found".
  - Replace old with new (all occurrences if all is true).
  - Write the modified content.
  - Return "ok".

### Glob Tool
Description: "Find files matching a pattern"
Required: pat (string)
Optional: path (string)

To execute:
  - Search for files matching the pattern.
  - Return up to 50 matches joined by newlines.

### Grep Tool  
Description: "Search for pattern in files"
Required: pat (string)
Optional: path (string)

To execute:
  - Search files for lines matching the pattern.
  - Return matches in format "file:line:content".

### Bash Tool
Description: "Execute a shell command"
Required: cmd (string)

To execute:
  - Run the command in a shell.
  - Return the output.

---

*Prose is a literate programming language where the code IS the documentation.*
*In the AI era, natural language becomes executable.*
