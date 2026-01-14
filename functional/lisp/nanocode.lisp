#!/usr/bin/env sbcl --script
;;; nanocode - minimal claude code alternative (Common Lisp)
(require :dexador)
(require :jonathan)

(defvar *key* (uiop:getenv "ANTHROPIC_API_KEY"))
(defvar *model* (or (uiop:getenv "MODEL") "claude-sonnet-4-20250514"))
(defvar *r* (format nil "~c[0m" #\Esc)) (defvar *b* (format nil "~c[1m" #\Esc))
(defvar *d* (format nil "~c[2m" #\Esc)) (defvar *c* (format nil "~c[36m" #\Esc))
(defvar *g* (format nil "~c[32m" #\Esc)) (defvar *bl* (format nil "~c[34m" #\Esc))

(defun tool (name input)
  (handler-case
      (cond
        ((string= name "read")
         (with-open-file (f (gethash "path" input))
           (format nil "~{~a~^~%~}" (loop for line = (read-line f nil) for i from 1 while line collect (format nil "~a| ~a" i line)))))
        ((string= name "write")
         (with-open-file (f (gethash "path" input) :direction :output :if-exists :supersede)
           (write-string (gethash "content" input) f)) "ok")
        ((string= name "bash")
         (uiop:run-program (gethash "cmd" input) :output :string))
        ((string= name "glob")
         (uiop:run-program (format nil "find . -name '~a' | head -50" (gethash "pat" input)) :output :string))
        ((string= name "grep")
         (uiop:run-program (format nil "grep -rn '~a' . | head -50" (gethash "pat" input)) :output :string))
        (t "unknown"))
    (error (e) (format nil "error: ~a" e))))

(defvar *schema* "[{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}},{\"name\":\"write\",\"description\":\"Write\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"}},\"required\":[\"path\",\"content\"]}},{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}},{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}},{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}]")

(defun ask (messages)
  (jonathan:parse
   (dex:post "https://api.anthropic.com/v1/messages"
             :headers `(("Content-Type" . "application/json") ("anthropic-version" . "2023-06-01") ("x-api-key" . ,*key*))
             :content (jonathan:to-json `(("model" . ,*model*) ("max_tokens" . 4096) ("system" . "Concise assistant") ("messages" . ,messages) ("tools" . ,(jonathan:parse *schema*)))))))

(format t "~ananocode~a | ~a~a~a~%~%" *b* *r* *d* *model* *r*)
(let ((messages '()))
  (loop
    (format t "~a~a❯~a " *b* *bl* *r*) (force-output)
    (let ((input (string-trim '(#\Space #\Tab #\Newline) (or (read-line *standard-input* nil) ""))))
      (when (string= input "") (loop))
      (when (string= input "/q") (return))
      (when (string= input "/c") (setf messages '()) (format t "~a⏺ Cleared~a~%" *g* *r*) (loop))
      
      (push `(("role" . "user") ("content" . ,input)) messages)
      (setf messages (nreverse messages))
      
      (loop
        (let* ((resp (ask messages))
               (content (gethash "content" resp))
               (results '()))
          (dolist (block content)
            (cond
              ((string= (gethash "type" block) "text")
               (format t "~%~a⏺~a ~a" *c* *r* (gethash "text" block)))
              ((string= (gethash "type" block) "tool_use")
               (let ((name (gethash "name" block)))
                 (format t "~%~a⏺ ~a~a~%" *g* name *r*)
                 (let ((result (tool name (gethash "input" block))))
                   (format t "  ~a⎿ ~a~a~%" *d* (car (uiop:split-string result :separator '(#\Newline))) *r*)
                   (push `(("type" . "tool_result") ("tool_use_id" . ,(gethash "id" block)) ("content" . ,result)) results))))))
          (push `(("role" . "assistant") ("content" . ,content)) messages)
          (when (null results) (return))
          (push `(("role" . "user") ("content" . ,(nreverse results))) messages)))
      (format t "~%"))))
