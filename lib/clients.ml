(** MockClaudeClient - Deterministic AI analysis via pattern matching *)

open Types

(* Helper: strip trailing punctuation from a word *)
let strip_punct (w : string) : string =
  let len = String.length w in
  if len = 0 then w
  else
    let last = w.[len - 1] in
    if last = '.' || last = ',' || last = ';' || last = ':' || last = '!' || last = '?' then
      String.sub w 0 (len - 1)
    else w

(* Helper: split string into words *)
let split_words (text : string) : string list =
  let buf = Buffer.create 64 in
  let words = ref [] in
  String.iter (fun c ->
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then begin
      if Buffer.length buf > 0 then begin
        words := strip_punct (Buffer.contents buf) :: !words;
        Buffer.clear buf
      end
    end else
      Buffer.add_char buf c
  ) text;
  if Buffer.length buf > 0 then
    words := strip_punct (Buffer.contents buf) :: !words;
  List.rev !words

(* Helper: split string into sentences *)
let split_sentences (text : string) : string list =
  let buf = Buffer.create 128 in
  let sentences = ref [] in
  String.iter (fun c ->
    Buffer.add_char buf c;
    if c = '.' || c = '!' || c = '?' then begin
      let s = String.trim (Buffer.contents buf) in
      if String.length s > 0 then
        sentences := s :: !sentences;
      Buffer.clear buf
    end
  ) text;
  let remaining = String.trim (Buffer.contents buf) in
  if String.length remaining > 0 then
    sentences := remaining :: !sentences;
  List.rev !sentences

(* Check for double spaces *)
let find_double_spaces (text : string) : grammar_issue list =
  let issues = ref [] in
  let len = String.length text in
  let i = ref 0 in
  while !i < len - 1 do
    if text.[!i] = ' ' && text.[!i + 1] = ' ' then begin
      issues := {
        position = !i;
        original = "  ";
        suggestion = " ";
        rule = "double_space";
      } :: !issues;
      (* Skip past this double space *)
      while !i < len - 1 && text.[!i] = ' ' && text.[!i + 1] = ' ' do
        i := !i + 1
      done
    end;
    i := !i + 1
  done;
  List.rev !issues

(* Check for missing capitalization after periods *)
let find_capitalization_issues (text : string) : grammar_issue list =
  let issues = ref [] in
  let len = String.length text in
  for i = 0 to len - 3 do
    if (text.[i] = '.' || text.[i] = '!' || text.[i] = '?')
       && text.[i + 1] = ' '
       && text.[i + 2] >= 'a' && text.[i + 2] <= 'z' then begin
      let lower_char = String.make 1 text.[i + 2] in
      let upper_char = String.uppercase_ascii lower_char in
      issues := {
        position = i + 2;
        original = lower_char;
        suggestion = upper_char;
        rule = "capitalization_after_sentence";
      } :: !issues
    end
  done;
  List.rev !issues

(* Check for text starting with lowercase *)
let find_start_capitalization (text : string) : grammar_issue list =
  let trimmed = String.trim text in
  if String.length trimmed > 0 && trimmed.[0] >= 'a' && trimmed.[0] <= 'z' then
    let lower_char = String.make 1 trimmed.[0] in
    let upper_char = String.uppercase_ascii lower_char in
    [{
      position = 0;
      original = lower_char;
      suggestion = upper_char;
      rule = "sentence_start_capitalization";
    }]
  else
    []

(* Check for missing period at end of text *)
let find_missing_end_punctuation (text : string) : grammar_issue list =
  let trimmed = String.trim text in
  let len = String.length trimmed in
  if len > 0 then
    let last = trimmed.[len - 1] in
    if last <> '.' && last <> '!' && last <> '?' then
      [{
        position = len - 1;
        original = String.make 1 last;
        suggestion = (String.make 1 last) ^ ".";
        rule = "missing_end_punctuation";
      }]
    else
      []
  else
    []

(* Check for repeated words like "the the" *)
let find_repeated_words (text : string) : grammar_issue list =
  let words = split_words text in
  let issues = ref [] in
  let pos = ref 0 in
  let prev = ref "" in
  List.iter (fun word ->
    let lower_word = String.lowercase_ascii word in
    let lower_prev = String.lowercase_ascii !prev in
    if String.length lower_prev > 0 && lower_word = lower_prev then begin
      issues := {
        position = !pos;
        original = !prev ^ " " ^ word;
        suggestion = word;
        rule = "repeated_word";
      } :: !issues
    end;
    pos := !pos + String.length word + 1;
    prev := word
  ) words;
  List.rev !issues

(** Run all grammar checks on text *)
let check_grammar (text : string) : grammar_issue list =
  let issues =
    find_double_spaces text
    @ find_capitalization_issues text
    @ find_start_capitalization text
    @ find_missing_end_punctuation text
    @ find_repeated_words text
  in
  (* Sort by position *)
  List.sort (fun a b -> compare a.position b.position) issues

(** Compute readability score *)
let compute_readability (text : string) : readability_score =
  let sentences = split_sentences text in
  let sentence_count = max 1 (List.length sentences) in
  let words = split_words text in
  let word_count = max 1 (List.length words) in
  let avg_sentence_length =
    float_of_int word_count /. float_of_int sentence_count
  in
  let total_word_chars =
    List.fold_left (fun acc w -> acc + String.length w) 0 words
  in
  let avg_word_length =
    float_of_int total_word_chars /. float_of_int word_count
  in
  (* Simplified Flesch-like score: higher = easier *)
  let score =
    206.835
    -. (1.015 *. avg_sentence_length)
    -. (84.6 *. (avg_word_length /. 5.0))
  in
  let score = Float.round (max 0.0 (min 100.0 score) *. 10.0) /. 10.0 in
  let level =
    if score >= 80.0 then "easy"
    else if score >= 60.0 then "moderate"
    else if score >= 40.0 then "difficult"
    else "very_difficult"
  in
  { sentence_count; avg_sentence_length; avg_word_length; score; level }

(** Detect passive voice patterns *)
let count_passive_voice (text : string) : int =
  let words = split_words text in
  let passive_helpers = ["is"; "was"; "were"; "are"; "been"; "being"; "be"] in
  let count = ref 0 in
  let prev = ref "" in
  List.iter (fun word ->
    let lower = String.lowercase_ascii word in
    (* Check if previous word was a passive helper and current ends in "ed" *)
    if List.mem (String.lowercase_ascii !prev) passive_helpers then begin
      let len = String.length lower in
      if len > 2 && String.sub lower (len - 2) 2 = "ed" then
        count := !count + 1
    end;
    prev := word
  ) words;
  !count

(** Count adverbs (words ending in -ly) *)
let count_adverbs (text : string) : int =
  let words = split_words text in
  List.fold_left (fun acc word ->
    let lower = String.lowercase_ascii word in
    let len = String.length lower in
    (* Exclude common non-adverb -ly words *)
    let exceptions = ["only"; "early"; "family"; "likely"; "lonely"; "friendly"; "ugly"; "daily"; "holy"; "july"; "fly"; "rely"; "apply"; "reply"; "supply"; "ally"] in
    if len > 2 && String.sub lower (len - 2) 2 = "ly"
       && not (List.mem lower exceptions) then
      acc + 1
    else
      acc
  ) 0 words

(** Compute sentence variety (std dev of sentence lengths) *)
let compute_sentence_variety (text : string) : float =
  let sentences = split_sentences text in
  let lengths = List.map (fun s -> float_of_int (List.length (split_words s))) sentences in
  let n = float_of_int (max 1 (List.length lengths)) in
  let mean = List.fold_left ( +. ) 0.0 lengths /. n in
  let variance =
    List.fold_left (fun acc l ->
      acc +. ((l -. mean) ** 2.0)
    ) 0.0 lengths /. n
  in
  Float.round (sqrt variance *. 100.0) /. 100.0

(** Analyze writing style *)
let analyze_style (text : string) : style_analysis =
  let passive_count = count_passive_voice text in
  let adverb_count = count_adverbs text in
  let sentence_variety = compute_sentence_variety text in
  let suggestions = ref [] in
  if passive_count > 2 then
    suggestions := "Reduce passive voice usage for stronger writing." :: !suggestions;
  if adverb_count > 3 then
    suggestions := "Consider reducing adverb usage; use stronger verbs instead." :: !suggestions;
  if sentence_variety < 2.0 then
    suggestions := "Vary your sentence lengths for better rhythm." :: !suggestions;
  if List.length (split_sentences text) < 3 then
    suggestions := "Consider breaking up long text into more sentences." :: !suggestions;
  {
    passive_count;
    adverb_count;
    sentence_variety;
    suggestions = List.rev !suggestions;
  }

(** Generate improved text *)
let improve_text (text : string) : text_improvement =
  let changes = ref [] in
  let improved = ref text in

  (* Fix double spaces *)
  let fix_double_spaces s =
    try
      let idx = Str.search_forward (Str.regexp "  ") s 0 in
      let _ = idx in
      let result = Str.global_replace (Str.regexp "  +") " " s in
      changes := "Removed extra spaces" :: !changes;
      result
    with Not_found -> s
  in
  (* We won't use Str - do it manually *)
  let fix_double_spaces_manual s =
    let buf = Buffer.create (String.length s) in
    let prev_space = ref false in
    let found = ref false in
    String.iter (fun c ->
      if c = ' ' then begin
        if not !prev_space then
          Buffer.add_char buf c
        else
          found := true;
        prev_space := true
      end else begin
        Buffer.add_char buf c;
        prev_space := false
      end
    ) s;
    if !found then
      changes := "Removed extra spaces" :: !changes;
    Buffer.contents buf
  in
  ignore fix_double_spaces;
  improved := fix_double_spaces_manual !improved;

  (* Fix start capitalization *)
  let s = String.trim !improved in
  if String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' then begin
    let first_upper = String.uppercase_ascii (String.sub s 0 1) in
    improved := first_upper ^ String.sub s 1 (String.length s - 1);
    changes := "Capitalized first letter" :: !changes
  end;

  (* Ensure ending punctuation *)
  let trimmed = String.trim !improved in
  let len = String.length trimmed in
  if len > 0 then begin
    let last = trimmed.[len - 1] in
    if last <> '.' && last <> '!' && last <> '?' then begin
      improved := trimmed ^ ".";
      changes := "Added ending punctuation" :: !changes
    end
  end;

  {
    original = text;
    improved = !improved;
    changes = List.rev !changes;
  }
