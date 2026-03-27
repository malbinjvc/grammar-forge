(** GrammarForge - AI Grammar & Syntax Analysis Service *)

let () =
  Dream.run
    ~port:8080
    ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Grammar_forge.Routes.router
