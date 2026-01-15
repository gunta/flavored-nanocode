(* nanocode - minimal claude code alternative (OCaml) *)
(* ocamlfind ocamlopt -package yojson,cohttp-lwt-unix,str -linkpkg nanocode.ml -o nanocode *)
open Lwt.Infix
open Str

let key =
  try Sys.getenv "OPENROUTER_API_KEY" with
  | Not_found -> Sys.getenv "ANTHROPIC_API_KEY"

let api =
  try Sys.getenv "API_URL" with
  | Not_found ->
      if Sys.getenv_opt "OPENROUTER_API_KEY" <> None then "https://openrouter.ai/api/v1/messages"
      else "https://api.anthropic.com/v1/messages"

let model =
  match Sys.getenv_opt "MODEL" with
  | Some m -> m
  | None ->
      if Sys.getenv_opt "OPENROUTER_API_KEY" <> None
      then "anthropic/claude-opus-4-5"
      else "claude-opus-4-5"
let r, b, d, c, g, bl = "\027[0m", "\027[1m", "\027[2m", "\027[36m", "\027[32m", "\027[34m"

let tool name input =
  let open Yojson.Basic.Util in
  match name with
  | "read" -> (try
      let lines = In_channel.with_open_text (input |> member "path" |> to_string) In_channel.input_all |> String.split_on_char '\n' in
      let off = input |> member "offset" |> to_int_option |> Option.value ~default:0 in
      let lim = input |> member "limit" |> to_int_option |> Option.value ~default:(List.length lines) in
      lines |> List.drop off |> fun l -> l |> List.take lim |> List.mapi (fun i l -> Printf.sprintf "%d| %s" (off+i+1) l) |> String.concat "\n"
    with _ -> "error")
  | "write" -> Out_channel.with_open_text (input |> member "path" |> to_string)
    (fun oc -> Out_channel.output_string oc (input |> member "content" |> to_string)); "ok"
  | "edit" -> (try
      let path = input |> member "path" |> to_string in
      let oldv = input |> member "old" |> to_string in
      let newv = input |> member "new" |> to_string in
      let all = input |> member "all" |> to_bool_option |> Option.value ~default:false in
      let txt = In_channel.with_open_text path In_channel.input_all in
      let occurrences = (split (regexp_string oldv) txt |> List.length) - 1 in
      if occurrences = 0 then "error: old_string not found"
      else if occurrences > 1 && not all then Printf.sprintf "error: old_string appears %d times, use all=true" occurrences
      else
        let updated =
          if all then Str.global_replace (Str.regexp_string oldv) newv txt
          else Str.replace_first (Str.regexp_string oldv) newv txt
        in
        Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc updated); "ok"
    with _ -> "error")
  | "bash" -> let ic = Unix.open_process_in (input |> member "cmd" |> to_string) in
    let r = In_channel.input_all ic in ignore (Unix.close_process_in ic); r
  | "glob" -> let ic = Unix.open_process_in (Printf.sprintf "find . -name '%s' | head -50" (input |> member "pat" |> to_string)) in
    let r = In_channel.input_all ic in ignore (Unix.close_process_in ic); r
  | "grep" -> let ic = Unix.open_process_in (Printf.sprintf "grep -rn '%s' . | head -50" (input |> member "pat" |> to_string)) in
    let r = In_channel.input_all ic in ignore (Unix.close_process_in ic); r
  | _ -> "unknown"

let schema = {|[
{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Replace","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}}
]|}

let ask messages =
  let body = `Assoc [
    "model", `String model; "max_tokens", `Int 4096; "system", `String "Concise assistant";
    "messages", messages; "tools", Yojson.Basic.from_string schema
  ] |> Yojson.Basic.to_string in
  let headers =
    let common = [ "Content-Type", "application/json"; "anthropic-version", "2023-06-01" ] in
    let auth =
      if Sys.getenv_opt "OPENROUTER_API_KEY" <> None
      then [ "Authorization", "Bearer " ^ key ]
      else [ "x-api-key", key ]
    in
    Cohttp.Header.of_list (common @ auth)
  in
  Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string api)
  >>= fun (_, body) -> Cohttp_lwt.Body.to_string body >|= Yojson.Basic.from_string

let () =
  Printf.printf "%snanocode%s | %s%s%s\n\n" b r d model r;
  let messages = ref (`List []) in
  try while true do
    Printf.printf "%s%s❯%s " b bl r; flush stdout;
    let input = String.trim (read_line ()) in
    if input = "" then () else if input = "/q" then exit 0
    else if input = "/c" then (messages := `List []; Printf.printf "%s⏺ Cleared%s\n" g r)
    else begin
      let open Yojson.Basic.Util in
      messages := `List (to_list !messages @ [`Assoc ["role", `String "user"; "content", `String input]]);
      let rec loop () =
        let resp = Lwt_main.run (ask !messages) in
        let content = resp |> member "content" |> to_list in
        let results = List.filter_map (fun block ->
          match block |> member "type" |> to_string with
          | "text" -> Printf.printf "\n%s⏺%s %s" c r (block |> member "text" |> to_string); None
          | "tool_use" ->
            let name = block |> member "name" |> to_string in
            Printf.printf "\n%s⏺ %s%s\n" g name r;
            let result = tool name (block |> member "input") in
            Printf.printf "  %s⎿ %s%s\n" d (String.split_on_char '\n' result |> List.hd) r;
            Some (`Assoc ["type", `String "tool_result"; "tool_use_id", block |> member "id"; "content", `String result])
          | _ -> None
        ) content in
        messages := `List (to_list !messages @ [`Assoc ["role", `String "assistant"; "content", `List content]]);
        if results = [] then () else begin
          messages := `List (to_list !messages @ [`Assoc ["role", `String "user"; "content", `List results]]);
          loop ()
        end
      in loop (); print_newline ()
    end
  done with End_of_file -> ()
