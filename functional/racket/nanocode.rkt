#!/usr/bin/env racket
#lang racket
;; nanocode - minimal claude code alternative (Racket)
(require net/http-easy json)

(define KEY (getenv "ANTHROPIC_API_KEY"))
(define MODEL (or (getenv "MODEL") "claude-sonnet-4-20250514"))
(define R "\033[0m") (define B "\033[1m") (define D "\033[2m") 
(define C "\033[36m") (define G "\033[32m") (define BL "\033[34m")

(define (tool name input)
  (match name
    ["read" (string-join (for/list ([l (file->lines (hash-ref input 'path))] [i (in-naturals 1)])
                           (format "~a| ~a" i l)) "\n")]
    ["write" (display-to-file (hash-ref input 'content) (hash-ref input 'path) #:exists 'replace) "ok"]
    ["edit" (let* ([p (hash-ref input 'path)] [t (file->string p)])
              (if (string-contains? t (hash-ref input 'old))
                  (begin (display-to-file (string-replace t (hash-ref input 'old) (hash-ref input 'new)) p #:exists 'replace) "ok")
                  "error: not found"))]
    ["glob" (with-output-to-string (λ () (system (format "find . -name '~a' | head -50" (hash-ref input 'pat)))))]
    ["grep" (with-output-to-string (λ () (system (format "grep -rn '~a' . | head -50" (hash-ref input 'pat)))))]
    ["bash" (with-output-to-string (λ () (system (hash-ref input 'cmd))))]
    [_ "unknown"]))

(define schema (string->jsexpr "[{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}},{\"name\":\"write\",\"description\":\"Write\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"}},\"required\":[\"path\",\"content\"]}},{\"name\":\"edit\",\"description\":\"Edit\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"old\":{\"type\":\"string\"},\"new\":{\"type\":\"string\"}},\"required\":[\"path\",\"old\",\"new\"]}},{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}},{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}},{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}}]"))

(define (ask messages)
  (response-json
   (post "https://api.anthropic.com/v1/messages"
         #:headers (hasheq 'Content-Type "application/json" 'anthropic-version "2023-06-01" 'x-api-key KEY)
         #:json (hasheq 'model MODEL 'max_tokens 4096 'system "Concise assistant" 'messages messages 'tools schema))))

(printf "~ananocode~a | ~a~a~a\n\n" B R D MODEL R)
(define messages '())

(let loop ()
  (printf "~a~a❯~a " B BL R) (flush-output)
  (define input (string-trim (or (read-line) "")))
  (cond
    [(eof-object? input) (void)]
    [(string=? input "") (loop)]
    [(string=? input "/q") (void)]
    [(string=? input "/c") (set! messages '()) (printf "~a⏺ Cleared~a\n" G R) (loop)]
    [else
     (set! messages (append messages (list (hasheq 'role "user" 'content input))))
     (let agent-loop ()
       (define resp (ask messages))
       (define content (hash-ref resp 'content))
       (define results
         (filter-map
          (λ (block)
            (cond
              [(string=? (hash-ref block 'type) "text")
               (printf "\n~a⏺~a ~a" C R (hash-ref block 'text)) #f]
              [(string=? (hash-ref block 'type) "tool_use")
               (define name (hash-ref block 'name))
               (printf "\n~a⏺ ~a~a\n" G name R)
               (define result (tool name (hash-ref block 'input)))
               (printf "  ~a⎿ ~a~a\n" D (car (string-split result "\n")) R)
               (hasheq 'type "tool_result" 'tool_use_id (hash-ref block 'id) 'content result)]
              [else #f]))
          content))
       (set! messages (append messages (list (hasheq 'role "assistant" 'content content))))
       (unless (null? results)
         (set! messages (append messages (list (hasheq 'role "user" 'content results))))
         (agent-loop)))
     (newline) (loop)]))
