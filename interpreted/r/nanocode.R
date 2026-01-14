#!/usr/bin/env Rscript
# nanocode - minimal claude code alternative (R)
library(httr)
library(jsonlite)

KEY <- Sys.getenv("ANTHROPIC_API_KEY")
MODEL <- ifelse(Sys.getenv("MODEL") == "", "claude-sonnet-4-20250514", Sys.getenv("MODEL"))
R <- "\033[0m"; B <- "\033[1m"; D <- "\033[2m"; C <- "\033[36m"; G <- "\033[32m"; BL <- "\033[34m"

tool <- function(name, input) {
  tryCatch({
    switch(name,
      "read" = paste(paste0(seq_along(readLines(input$path)), "| ", readLines(input$path)), collapse = "\n"),
      "write" = { writeLines(input$content, input$path); "ok" },
      "edit" = {
        t <- paste(readLines(input$path), collapse = "\n")
        if (!grepl(input$old, t, fixed = TRUE)) return("error: not found")
        writeLines(sub(input$old, input$new, t, fixed = TRUE), input$path); "ok"
      },
      "glob" = paste(head(system(sprintf("find . -name '%s'", input$pat), intern = TRUE), 50), collapse = "\n"),
      "grep" = paste(head(system(sprintf("grep -rn '%s' .", input$pat), intern = TRUE), 50), collapse = "\n"),
      "bash" = paste(system(input$cmd, intern = TRUE), collapse = "\n"),
      "unknown"
    )
  }, error = function(e) paste("error:", e$message))
}

schema <- fromJSON('[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]')

ask <- function(messages) {
  resp <- POST("https://api.anthropic.com/v1/messages",
    add_headers(`Content-Type` = "application/json", `anthropic-version` = "2023-06-01", `x-api-key` = KEY),
    body = toJSON(list(model = MODEL, max_tokens = 4096, system = "Concise assistant", messages = messages, tools = schema), auto_unbox = TRUE),
    encode = "raw")
  fromJSON(content(resp, "text", encoding = "UTF-8"))
}

cat(sprintf("%snanocode%s | %s%s%s\n\n", B, R, D, MODEL, R))
messages <- list()

repeat {
  cat(sprintf("%s%s❯%s ", B, BL, R))
  input <- trimws(readLines(con = "stdin", n = 1))
  if (length(input) == 0 || input == "/q") break
  if (input == "") next
  if (input == "/c") { messages <- list(); cat(sprintf("%s⏺ Cleared%s\n", G, R)); next }

  messages <- c(messages, list(list(role = "user", content = input)))

  repeat {
    resp <- ask(messages)
    content <- resp$content
    results <- list()

    for (i in seq_len(nrow(content))) {
      block <- content[i, ]
      if (block$type == "text") cat(sprintf("\n%s⏺%s %s", C, R, block$text))
      if (block$type == "tool_use") {
        name <- block$name
        cat(sprintf("\n%s⏺ %s%s\n", G, name, R))
        result <- tool(name, block$input)
        cat(sprintf("  %s⎿ %s%s\n", D, strsplit(result, "\n")[[1]][1], R))
        results <- c(results, list(list(type = "tool_result", tool_use_id = block$id, content = result)))
      }
    }

    messages <- c(messages, list(list(role = "assistant", content = content)))
    if (length(results) == 0) break
    messages <- c(messages, list(list(role = "user", content = results)))
  }
  cat("\n")
}
