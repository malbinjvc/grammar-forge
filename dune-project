(lang dune 3.0)
(name grammar_forge)
(generate_opam_files true)

(package
 (name grammar_forge)
 (allow_empty)
 (synopsis "AI-powered grammar and syntax analysis service")
 (depends
  (ocaml (>= 5.3.0))
  (dream (>= 1.0.0~alpha8))
  (yojson (>= 2.0.0))
  (alcotest :with-test)))
