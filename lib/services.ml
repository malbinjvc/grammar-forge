(** Business logic services for GrammarForge *)

open Types

(* In-memory document storage *)
let document_store : (string, document) Hashtbl.t = Hashtbl.create 64

(* Track analysis results for stats *)
let issues_count : (string, int) Hashtbl.t = Hashtbl.create 64
let readability_scores : (string, float) Hashtbl.t = Hashtbl.create 64

(* Simple ID counter *)
let next_id = ref 0

let generate_id () : string =
  next_id := !next_id + 1;
  Printf.sprintf "doc_%d" !next_id

let current_timestamp () : string =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Reset all state - useful for testing *)
let reset () =
  Hashtbl.clear document_store;
  Hashtbl.clear issues_count;
  Hashtbl.clear readability_scores;
  next_id := 0

(** Create a new document *)
let create_document (title : string) (content : string) : document =
  let id = generate_id () in
  let doc = {
    id;
    title;
    content;
    created_at = current_timestamp ();
  } in
  Hashtbl.replace document_store id doc;
  (* Auto-analyze *)
  let issues = Clients.check_grammar content in
  Hashtbl.replace issues_count id (List.length issues);
  let readability = Clients.compute_readability content in
  Hashtbl.replace readability_scores id readability.score;
  doc

(** Get all documents *)
let list_documents () : document list =
  Hashtbl.fold (fun _key doc acc -> doc :: acc) document_store []

(** Get a document by ID *)
let get_document (id : string) : document option =
  Hashtbl.find_opt document_store id

(** Delete a document by ID *)
let delete_document (id : string) : bool =
  match Hashtbl.find_opt document_store id with
  | Some _ ->
    Hashtbl.remove document_store id;
    Hashtbl.remove issues_count id;
    Hashtbl.remove readability_scores id;
    true
  | None -> false

(** Check grammar for text *)
let check_grammar (text : string) : grammar_issue list =
  Clients.check_grammar text

(** Score readability *)
let score_readability (text : string) : readability_score =
  Clients.compute_readability text

(** Analyze style *)
let analyze_style (text : string) : style_analysis =
  Clients.analyze_style text

(** Improve text *)
let improve_text (text : string) : text_improvement =
  Clients.improve_text text

(** Get statistics *)
let get_stats () : stats =
  let docs_count = Hashtbl.length document_store in
  let total_issues =
    Hashtbl.fold (fun _k v acc -> acc + v) issues_count 0
  in
  let total_readability =
    Hashtbl.fold (fun _k v acc -> acc +. v) readability_scores 0.0
  in
  let avg_readability =
    if docs_count > 0 then
      Float.round (total_readability /. float_of_int docs_count *. 10.0) /. 10.0
    else 0.0
  in
  {
    documents_analyzed = docs_count;
    total_issues_found = total_issues;
    avg_readability_score = avg_readability;
  }
