(** Core data types for GrammarForge *)

type document = {
  id : string;
  title : string;
  content : string;
  created_at : string;
}

type grammar_issue = {
  position : int;
  original : string;
  suggestion : string;
  rule : string;
}

type readability_score = {
  sentence_count : int;
  avg_sentence_length : float;
  avg_word_length : float;
  score : float;
  level : string;
}

type style_analysis = {
  passive_count : int;
  adverb_count : int;
  sentence_variety : float;
  suggestions : string list;
}

type text_improvement = {
  original : string;
  improved : string;
  changes : string list;
}

type stats = {
  documents_analyzed : int;
  total_issues_found : int;
  avg_readability_score : float;
}

(* JSON serialization *)

let document_to_json (doc : document) : Yojson.Safe.t =
  `Assoc [
    ("id", `String doc.id);
    ("title", `String doc.title);
    ("content", `String doc.content);
    ("created_at", `String doc.created_at);
  ]

let document_of_json (json : Yojson.Safe.t) : (document, string) result =
  try
    let open Yojson.Safe.Util in
    let title = json |> member "title" |> to_string in
    let content = json |> member "content" |> to_string in
    Ok {
      id = "";
      title;
      content;
      created_at = "";
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
  | _ -> Error "Invalid JSON format"

let grammar_issue_to_json (issue : grammar_issue) : Yojson.Safe.t =
  `Assoc [
    ("position", `Int issue.position);
    ("original", `String issue.original);
    ("suggestion", `String issue.suggestion);
    ("rule", `String issue.rule);
  ]

let readability_score_to_json (rs : readability_score) : Yojson.Safe.t =
  `Assoc [
    ("sentence_count", `Int rs.sentence_count);
    ("avg_sentence_length", `Float rs.avg_sentence_length);
    ("avg_word_length", `Float rs.avg_word_length);
    ("score", `Float rs.score);
    ("level", `String rs.level);
  ]

let style_analysis_to_json (sa : style_analysis) : Yojson.Safe.t =
  `Assoc [
    ("passive_count", `Int sa.passive_count);
    ("adverb_count", `Int sa.adverb_count);
    ("sentence_variety", `Float sa.sentence_variety);
    ("suggestions", `List (List.map (fun s -> `String s) sa.suggestions));
  ]

let text_improvement_to_json (ti : text_improvement) : Yojson.Safe.t =
  `Assoc [
    ("original", `String ti.original);
    ("improved", `String ti.improved);
    ("changes", `List (List.map (fun s -> `String s) ti.changes));
  ]

let stats_to_json (s : stats) : Yojson.Safe.t =
  `Assoc [
    ("documents_analyzed", `Int s.documents_analyzed);
    ("total_issues_found", `Int s.total_issues_found);
    ("avg_readability_score", `Float s.avg_readability_score);
  ]

let text_of_json (json : Yojson.Safe.t) : (string, string) result =
  try
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    Ok text
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
  | _ -> Error "Invalid JSON format: missing 'text' field"
