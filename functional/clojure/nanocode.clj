#!/usr/bin/env bb
; nanocode - minimal claude code alternative (Clojure/Babashka)
(require '[babashka.http-client :as http] '[cheshire.core :as json] '[clojure.string :as str])

(def KEY (System/getenv "ANTHROPIC_API_KEY"))
(def MODEL (or (System/getenv "MODEL") "claude-sonnet-4-20250514"))
(def R "\033[0m") (def B "\033[1m") (def D "\033[2m") (def C "\033[36m") (def G "\033[32m") (def BL "\033[34m")

(def tools
  {"read" #(str/join "\n" (map-indexed (fn [i l] (str (inc i) "| " l)) (str/split-lines (slurp (:path %)))))
   "write" #(do (spit (:path %) (:content %)) "ok")
   "edit" #(let [t (slurp (:path %))] (if (str/includes? t (:old %)) (do (spit (:path %) (str/replace-first t (:old %) (:new %))) "ok") "error: not found"))
   "glob" #(str/join "\n" (take 50 (str/split-lines (:out (clojure.java.shell/sh "find" "." "-name" (:pat %))))))
   "grep" #(str/join "\n" (take 50 (str/split-lines (:out (clojure.java.shell/sh "grep" "-rn" (:pat %) ".")))))
   "bash" #(:out (clojure.java.shell/sh "sh" "-c" (:cmd %)))})

(def schema [{"name" "read" "description" "Read" "input_schema" {"type" "object" "properties" {"path" {"type" "string"}} "required" ["path"]}}
             {"name" "write" "description" "Write" "input_schema" {"type" "object" "properties" {"path" {"type" "string"} "content" {"type" "string"}} "required" ["path" "content"]}}
             {"name" "edit" "description" "Edit" "input_schema" {"type" "object" "properties" {"path" {"type" "string"} "old" {"type" "string"} "new" {"type" "string"}} "required" ["path" "old" "new"]}}
             {"name" "glob" "description" "Find" "input_schema" {"type" "object" "properties" {"pat" {"type" "string"}} "required" ["pat"]}}
             {"name" "grep" "description" "Search" "input_schema" {"type" "object" "properties" {"pat" {"type" "string"}} "required" ["pat"]}}
             {"name" "bash" "description" "Run" "input_schema" {"type" "object" "properties" {"cmd" {"type" "string"}} "required" ["cmd"]}}])

(defn ask [messages]
  (-> (http/post "https://api.anthropic.com/v1/messages"
        {:headers {"Content-Type" "application/json" "anthropic-version" "2023-06-01" "x-api-key" KEY}
         :body (json/generate-string {:model MODEL :max_tokens 4096 :system "Concise assistant" :messages messages :tools schema})})
      :body (json/parse-string true)))

(println (str B "nanocode" R " | " D MODEL R "\n"))

(loop [messages []]
  (print (str B BL "❯" R " ")) (flush)
  (when-let [input (some-> (read-line) str/trim)]
    (cond
      (= input "/q") nil
      (empty? input) (recur messages)
      (= input "/c") (do (println (str G "⏺ Cleared" R)) (recur []))
      :else
      (let [messages (conj messages {:role "user" :content input})]
        (loop [messages messages]
          (let [{:keys [content]} (ask messages)
                results (atom [])]
            (doseq [block content]
              (when (= (:type block) "text") (println (str "\n" C "⏺" R " " (:text block))))
              (when (= (:type block) "tool_use")
                (println (str "\n" G "⏺ " (:name block) R))
                (let [result ((tools (:name block)) (:input block))]
                  (println (str "  " D "⎿ " (first (str/split-lines result)) R))
                  (swap! results conj {:type "tool_result" :tool_use_id (:id block) :content result}))))
            (let [messages (conj messages {:role "assistant" :content content})]
              (if (empty? @results)
                (do (println) (recur messages))
                (recur (conj messages {:role "user" :content @results}))))))))))
