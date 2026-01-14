(* nanocode - minimal claude code alternative (Coq) *)
(* Note: Coq is primarily a proof assistant, but can extract to OCaml *)
(* coqc nanocode.v && ocamlopt -o nanocode nanocode.ml *)

Require Import String.
Require Import List.
Import ListNotations.

(* Types for our nanocode *)
Inductive ToolName := Read | Write | Bash | Glob | Grep.

Record ToolInput := mkInput {
  path : option string;
  content : option string;
  cmd : option string;
  pat : option string
}.

Inductive Result :=
  | Ok : string -> Result
  | Error : string -> Result.

(* Tool specification - what each tool does *)
Definition tool_spec (name : ToolName) : string :=
  match name with
  | Read => "Read file contents with line numbers"
  | Write => "Write content to a file"
  | Bash => "Execute a shell command"
  | Glob => "Find files matching a pattern"
  | Grep => "Search for pattern in files"
  end.

(* The actual implementation would use extraction to OCaml *)
(* Here we define the logic that will be extracted *)

Definition read_file (p : string) : Result := Ok "1| file content".
Definition write_file (p c : string) : Result := Ok "ok".
Definition run_bash (c : string) : Result := Ok "command output".

Definition run_tool (name : ToolName) (input : ToolInput) : Result :=
  match name with
  | Read => match path input with
            | Some p => read_file p
            | None => Error "no path"
            end
  | Write => match path input, content input with
             | Some p, Some c => write_file p c
             | _, _ => Error "missing args"
             end
  | Bash => match cmd input with
            | Some c => run_bash c
            | None => Error "no command"
            end
  | _ => Error "not implemented"
  end.

(* REPL state *)
Record State := mkState {
  messages : list (string * string);
  running : bool
}.

Definition initial_state : State := mkState [] true.

(* Prove that read_file always returns a result *)
Theorem read_always_returns : forall p, exists r, read_file p = r.
Proof.
  intros p. exists (Ok "1| file content"). reflexivity.
Qed.

(* Prove tool execution is total *)
Theorem tool_total : forall n i, exists r, run_tool n i = r.
Proof.
  intros n i.
  destruct n; simpl; destruct (path i); try destruct (content i);
  try destruct (cmd i); eexists; reflexivity.
Qed.

(* Extract to OCaml for actual execution *)
Extraction Language OCaml.
Extract Inlined Constant read_file => "fun p -> Ok (String.concat \"\n\" (List.mapi (fun i l -> string_of_int (i+1) ^ \"| \" ^ l) (String.split_on_char '\n' (In_channel.with_open_text p In_channel.input_all))))".
