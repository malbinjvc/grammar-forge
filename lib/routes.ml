(** Dream HTTP routes for GrammarForge *)

open Types

let json_response ?(status = `OK) (json : Yojson.Safe.t) =
  Dream.json ~status (Yojson.Safe.to_string json)

let error_response ?(status = `Bad_Request) (msg : string) =
  json_response ~status (`Assoc [("error", `String msg)])

(** POST /api/documents *)
let create_document_handler request =
  let%lwt body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response "Invalid JSON"
  | json ->
    match document_of_json json with
    | Error msg -> error_response msg
    | Ok partial ->
      let doc = Services.create_document partial.title partial.content in
      json_response ~status:`Created (document_to_json doc)

(** GET /api/documents *)
let list_documents_handler _request =
  let docs = Services.list_documents () in
  let json_docs = List.map document_to_json docs in
  json_response (`Assoc [
    ("documents", `List json_docs);
    ("count", `Int (List.length docs));
  ])

(** GET /api/documents/:id *)
let get_document_handler request =
  let id = Dream.param request "id" in
  match Services.get_document id with
  | None -> error_response ~status:`Not_Found "Document not found"
  | Some doc ->
    let issues = Services.check_grammar doc.content in
    let readability = Services.score_readability doc.content in
    let style = Services.analyze_style doc.content in
    json_response (`Assoc [
      ("document", document_to_json doc);
      ("grammar_issues", `List (List.map grammar_issue_to_json issues));
      ("readability", readability_score_to_json readability);
      ("style", style_analysis_to_json style);
    ])

(** DELETE /api/documents/:id *)
let delete_document_handler request =
  let id = Dream.param request "id" in
  if Services.delete_document id then
    json_response (`Assoc [("message", `String "Document deleted")])
  else
    error_response ~status:`Not_Found "Document not found"

(** POST /api/grammar/check *)
let grammar_check_handler request =
  let%lwt body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response "Invalid JSON"
  | json ->
    match text_of_json json with
    | Error msg -> error_response msg
    | Ok text ->
      let issues = Services.check_grammar text in
      json_response (`Assoc [
        ("issues", `List (List.map grammar_issue_to_json issues));
        ("issue_count", `Int (List.length issues));
      ])

(** POST /api/readability/score *)
let readability_score_handler request =
  let%lwt body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response "Invalid JSON"
  | json ->
    match text_of_json json with
    | Error msg -> error_response msg
    | Ok text ->
      let score = Services.score_readability text in
      json_response (readability_score_to_json score)

(** POST /api/style/analyze *)
let style_analyze_handler request =
  let%lwt body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response "Invalid JSON"
  | json ->
    match text_of_json json with
    | Error msg -> error_response msg
    | Ok text ->
      let analysis = Services.analyze_style text in
      json_response (style_analysis_to_json analysis)

(** GET /api/stats *)
let stats_handler _request =
  let stats = Services.get_stats () in
  json_response (stats_to_json stats)

(** POST /api/text/improve *)
let improve_handler request =
  let%lwt body = Dream.body request in
  match Yojson.Safe.from_string body with
  | exception _ -> error_response "Invalid JSON"
  | json ->
    match text_of_json json with
    | Error msg -> error_response msg
    | Ok text ->
      let improvement = Services.improve_text text in
      json_response (text_improvement_to_json improvement)

(** GET /health *)
let health_handler _request =
  json_response (`Assoc [
    ("status", `String "healthy");
    ("service", `String "grammar-forge");
    ("version", `String "1.0.0");
  ])

(** Build Dream router *)
let router =
  Dream.router [
    Dream.post "/api/documents" create_document_handler;
    Dream.get "/api/documents" list_documents_handler;
    Dream.get "/api/documents/:id" get_document_handler;
    Dream.delete "/api/documents/:id" delete_document_handler;
    Dream.post "/api/grammar/check" grammar_check_handler;
    Dream.post "/api/readability/score" readability_score_handler;
    Dream.post "/api/style/analyze" style_analyze_handler;
    Dream.get "/api/stats" stats_handler;
    Dream.post "/api/text/improve" improve_handler;
    Dream.get "/health" health_handler;
  ]
