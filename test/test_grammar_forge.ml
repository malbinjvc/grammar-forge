(** Tests for GrammarForge *)

open Grammar_forge
open Alcotest

(* ===== Types tests ===== *)

let test_document_to_json () =
  let doc : Types.document = {
    id = "doc_1";
    title = "Test";
    content = "Hello world.";
    created_at = "2026-03-26T00:00:00Z";
  } in
  let json = Types.document_to_json doc in
  let str = Yojson.Safe.to_string json in
  check bool "contains id" true (String.length str > 0);
  let open Yojson.Safe.Util in
  check string "id field" "doc_1" (json |> member "id" |> to_string);
  check string "title field" "Test" (json |> member "title" |> to_string)

let test_document_of_json_valid () =
  let json = Yojson.Safe.from_string {|{"title":"My Doc","content":"Some text."}|} in
  match Types.document_of_json json with
  | Ok doc ->
    check string "title" "My Doc" doc.title;
    check string "content" "Some text." doc.content
  | Error msg -> fail msg

let test_document_of_json_invalid () =
  let json = Yojson.Safe.from_string {|{"bad":"data"}|} in
  match Types.document_of_json json with
  | Ok _ -> fail "Should have failed"
  | Error _ -> check pass "returns error" () ()

let test_text_of_json_valid () =
  let json = Yojson.Safe.from_string {|{"text":"Hello world."}|} in
  match Types.text_of_json json with
  | Ok text -> check string "text" "Hello world." text
  | Error msg -> fail msg

let test_text_of_json_invalid () =
  let json = Yojson.Safe.from_string {|{"wrong":"field"}|} in
  match Types.text_of_json json with
  | Ok _ -> fail "Should have failed"
  | Error _ -> check pass "returns error" () ()

(* ===== Clients / MockClaude tests ===== *)

let test_check_grammar_double_spaces () =
  let issues = Clients.check_grammar "Hello  world." in
  let has_double_space = List.exists (fun (i : Types.grammar_issue) ->
    i.rule = "double_space"
  ) issues in
  check bool "finds double space" true has_double_space

let test_check_grammar_capitalization () =
  let issues = Clients.check_grammar "Hello. world is nice." in
  let has_cap = List.exists (fun (i : Types.grammar_issue) ->
    i.rule = "capitalization_after_sentence"
  ) issues in
  check bool "finds capitalization issue" true has_cap

let test_check_grammar_start_lowercase () =
  let issues = Clients.check_grammar "hello world." in
  let has_start = List.exists (fun (i : Types.grammar_issue) ->
    i.rule = "sentence_start_capitalization"
  ) issues in
  check bool "finds start capitalization" true has_start

let test_check_grammar_missing_punctuation () =
  let issues = Clients.check_grammar "Hello world" in
  let has_punct = List.exists (fun (i : Types.grammar_issue) ->
    i.rule = "missing_end_punctuation"
  ) issues in
  check bool "finds missing punctuation" true has_punct

let test_check_grammar_repeated_words () =
  let issues = Clients.check_grammar "The the cat sat." in
  let has_repeated = List.exists (fun (i : Types.grammar_issue) ->
    i.rule = "repeated_word"
  ) issues in
  check bool "finds repeated word" true has_repeated

let test_check_grammar_clean_text () =
  let issues = Clients.check_grammar "Hello world." in
  check int "no issues for clean text" 0 (List.length issues)

let test_readability_easy () =
  let score = Clients.compute_readability "I am good. You are nice. We are happy." in
  check bool "easy text score >= 60" true (score.score >= 60.0);
  check bool "level is easy or moderate" true
    (score.level = "easy" || score.level = "moderate")

let test_readability_sentence_count () =
  let score = Clients.compute_readability "One. Two. Three." in
  check int "three sentences" 3 score.sentence_count

let test_readability_score_range () =
  let score = Clients.compute_readability "Testing the readability scoring system." in
  check bool "score >= 0" true (score.score >= 0.0);
  check bool "score <= 100" true (score.score <= 100.0)

let test_passive_voice_detection () =
  let count = Clients.count_passive_voice "The ball was kicked by him. The cake was baked." in
  check bool "detects passive voice" true (count >= 2)

let test_no_passive_voice () =
  let count = Clients.count_passive_voice "He kicked the ball." in
  check int "no passive voice" 0 count

let test_adverb_count () =
  let count = Clients.count_adverbs "He quickly and silently walked carefully." in
  check bool "finds adverbs" true (count >= 3)

let test_adverb_exceptions () =
  let count = Clients.count_adverbs "The family went early to the only store." in
  check int "excludes non-adverb ly words" 0 count

let test_style_analysis () =
  let analysis = Clients.analyze_style "The ball was kicked. The cake was baked. The song was sung. He quickly ran." in
  check bool "passive count > 0" true (analysis.passive_count > 0);
  check bool "has suggestions" true (List.length analysis.suggestions > 0)

let test_sentence_variety () =
  let variety = Clients.compute_sentence_variety "Short. This is a much longer sentence with many words." in
  check bool "variety > 0 for mixed lengths" true (variety > 0.0)

let test_improve_text_double_spaces () =
  let result = Clients.improve_text "Hello  world" in
  check bool "no double spaces in improved" true
    (not (try let _ = Str.search_forward (Str.regexp "  ") result.improved 0 in true with Not_found -> false));
  check bool "has changes" true (List.length result.changes > 0)

let test_improve_text_capitalization () =
  let result = Clients.improve_text "hello world." in
  check bool "capitalized" true (result.improved.[0] >= 'A' && result.improved.[0] <= 'Z')

let test_improve_text_punctuation () =
  let result = Clients.improve_text "Hello world" in
  let len = String.length result.improved in
  check bool "ends with period" true (result.improved.[len - 1] = '.')

(* Helper for String.contains_s which doesn't exist *)
let () =
  (* This is just to make the test compile - we use a different check *)
  ignore (fun () -> ())

(* ===== Services tests ===== *)

let test_create_document () =
  Services.reset ();
  let doc = Services.create_document "Test Title" "Hello world." in
  check string "title" "Test Title" doc.title;
  check string "content" "Hello world." doc.content;
  check bool "id not empty" true (String.length doc.id > 0)

let test_list_documents () =
  Services.reset ();
  ignore (Services.create_document "Doc 1" "Content one.");
  ignore (Services.create_document "Doc 2" "Content two.");
  let docs = Services.list_documents () in
  check int "two documents" 2 (List.length docs)

let test_get_document () =
  Services.reset ();
  let doc = Services.create_document "Test" "Content." in
  match Services.get_document doc.id with
  | Some found -> check string "same title" "Test" found.title
  | None -> fail "Document not found"

let test_get_document_not_found () =
  Services.reset ();
  match Services.get_document "nonexistent" with
  | Some _ -> fail "Should not find"
  | None -> check pass "returns none" () ()

let test_delete_document () =
  Services.reset ();
  let doc = Services.create_document "Test" "Content." in
  check bool "delete succeeds" true (Services.delete_document doc.id);
  check bool "no longer exists" true (Services.get_document doc.id = None)

let test_delete_nonexistent () =
  Services.reset ();
  check bool "delete fails" false (Services.delete_document "nonexistent")

let test_stats_empty () =
  Services.reset ();
  let stats = Services.get_stats () in
  check int "zero docs" 0 stats.documents_analyzed;
  check int "zero issues" 0 stats.total_issues_found

let test_stats_with_docs () =
  Services.reset ();
  ignore (Services.create_document "Doc 1" "hello  world");
  ignore (Services.create_document "Doc 2" "Good text.");
  let stats = Services.get_stats () in
  check int "two docs" 2 stats.documents_analyzed;
  check bool "has issues" true (stats.total_issues_found > 0)

(* ===== HTTP route tests via Dream.test ===== *)

let test_health_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`GET ~target:"/health" "" in
  let response = Dream.test handler request in
  let status = Dream.status response in
  check int "200 OK" 200 (Dream.status_to_int status);
  let body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  check string "status healthy" "healthy" (json |> member "status" |> to_string)

let test_create_document_endpoint () =
  Services.reset ();
  let handler = Grammar_forge.Routes.router in
  let body = {|{"title":"API Test","content":"Hello world."}|} in
  let request = Dream.request ~method_:`POST ~target:"/api/documents" body in
  let response = Dream.test handler request in
  let status = Dream.status response in
  check int "201 Created" 201 (Dream.status_to_int status);
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check string "title" "API Test" (json |> member "title" |> to_string)

let test_list_documents_endpoint () =
  Services.reset ();
  ignore (Services.create_document "D1" "Content.");
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`GET ~target:"/api/documents" "" in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  check int "count 1" 1 (json |> member "count" |> to_int)

let test_grammar_check_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let body = {|{"text":"hello  world"}|} in
  let request = Dream.request ~method_:`POST ~target:"/api/grammar/check" body in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check bool "has issues" true (json |> member "issue_count" |> to_int > 0)

let test_readability_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let body = {|{"text":"Simple text. Easy to read. Short sentences."}|} in
  let request = Dream.request ~method_:`POST ~target:"/api/readability/score" body in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check bool "has score" true (json |> member "score" |> to_float >= 0.0)

let test_style_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let body = {|{"text":"The ball was kicked. He ran quickly."}|} in
  let request = Dream.request ~method_:`POST ~target:"/api/style/analyze" body in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check bool "has passive_count" true (json |> member "passive_count" |> to_int >= 0)

let test_improve_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let body = {|{"text":"hello  world"}|} in
  let request = Dream.request ~method_:`POST ~target:"/api/text/improve" body in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check bool "has improved" true (String.length (json |> member "improved" |> to_string) > 0)

let test_stats_endpoint () =
  Services.reset ();
  ignore (Services.create_document "S" "Test content.");
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`GET ~target:"/api/stats" "" in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response));
  let resp_body = Lwt_main.run (Dream.body response) in
  let json = Yojson.Safe.from_string resp_body in
  let open Yojson.Safe.Util in
  check int "1 doc" 1 (json |> member "documents_analyzed" |> to_int)

let test_delete_document_endpoint () =
  Services.reset ();
  let doc = Services.create_document "Del" "To delete." in
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`DELETE ~target:("/api/documents/" ^ doc.id) "" in
  let response = Dream.test handler request in
  check int "200 OK" 200 (Dream.status_to_int (Dream.status response))

let test_not_found_document_endpoint () =
  Services.reset ();
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`GET ~target:"/api/documents/nonexistent" "" in
  let response = Dream.test handler request in
  check int "404" 404 (Dream.status_to_int (Dream.status response))

let test_invalid_json_endpoint () =
  let handler = Grammar_forge.Routes.router in
  let request = Dream.request ~method_:`POST ~target:"/api/grammar/check" "not json" in
  let response = Dream.test handler request in
  check int "400" 400 (Dream.status_to_int (Dream.status response))

(* ===== Test runner ===== *)

let () =
  run "GrammarForge" [
    "types", [
      test_case "document_to_json" `Quick test_document_to_json;
      test_case "document_of_json valid" `Quick test_document_of_json_valid;
      test_case "document_of_json invalid" `Quick test_document_of_json_invalid;
      test_case "text_of_json valid" `Quick test_text_of_json_valid;
      test_case "text_of_json invalid" `Quick test_text_of_json_invalid;
    ];
    "grammar", [
      test_case "double spaces" `Quick test_check_grammar_double_spaces;
      test_case "capitalization" `Quick test_check_grammar_capitalization;
      test_case "start lowercase" `Quick test_check_grammar_start_lowercase;
      test_case "missing punctuation" `Quick test_check_grammar_missing_punctuation;
      test_case "repeated words" `Quick test_check_grammar_repeated_words;
      test_case "clean text" `Quick test_check_grammar_clean_text;
    ];
    "readability", [
      test_case "easy text" `Quick test_readability_easy;
      test_case "sentence count" `Quick test_readability_sentence_count;
      test_case "score range" `Quick test_readability_score_range;
    ];
    "style", [
      test_case "passive voice" `Quick test_passive_voice_detection;
      test_case "no passive" `Quick test_no_passive_voice;
      test_case "adverb count" `Quick test_adverb_count;
      test_case "adverb exceptions" `Quick test_adverb_exceptions;
      test_case "style analysis" `Quick test_style_analysis;
      test_case "sentence variety" `Quick test_sentence_variety;
    ];
    "improvement", [
      test_case "fix double spaces" `Quick test_improve_text_double_spaces;
      test_case "fix capitalization" `Quick test_improve_text_capitalization;
      test_case "fix punctuation" `Quick test_improve_text_punctuation;
    ];
    "services", [
      test_case "create document" `Quick test_create_document;
      test_case "list documents" `Quick test_list_documents;
      test_case "get document" `Quick test_get_document;
      test_case "get nonexistent" `Quick test_get_document_not_found;
      test_case "delete document" `Quick test_delete_document;
      test_case "delete nonexistent" `Quick test_delete_nonexistent;
      test_case "stats empty" `Quick test_stats_empty;
      test_case "stats with docs" `Quick test_stats_with_docs;
    ];
    "http", [
      test_case "health" `Quick test_health_endpoint;
      test_case "create document" `Quick test_create_document_endpoint;
      test_case "list documents" `Quick test_list_documents_endpoint;
      test_case "grammar check" `Quick test_grammar_check_endpoint;
      test_case "readability" `Quick test_readability_endpoint;
      test_case "style" `Quick test_style_endpoint;
      test_case "improve" `Quick test_improve_endpoint;
      test_case "stats" `Quick test_stats_endpoint;
      test_case "delete" `Quick test_delete_document_endpoint;
      test_case "not found" `Quick test_not_found_document_endpoint;
      test_case "invalid json" `Quick test_invalid_json_endpoint;
    ];
  ]
