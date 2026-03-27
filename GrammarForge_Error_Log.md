# GrammarForge Error Log

## Project: GrammarForge
- **Language**: OCaml 5.3.0
- **Framework**: Dream 1.0.0~alpha8
- **Date**: 2026-03-27

---

## Error 1: dune allow_empty Error

**Error**: `Error: Package grammar_forge has no user-defined stanzas`

**Cause**: The dune-project package definition had no stanzas, which newer dune versions reject by default.

**Fix**: Added `(allow_empty)` to the package definition in `dune-project`.

---

## Error 2: Unused rec Flag

**Error**: Warning about unused `rec` keyword on `let rec fix_double_spaces`.

**Cause**: The function `fix_double_spaces` was defined with `let rec` but never called itself recursively.

**Fix**: Removed the `rec` keyword, keeping `let fix_double_spaces`.

---

## Error 3: Missing lwt_ppx Preprocessor

**Error**: `Uninterpreted extension 'lwt'` when compiling Dream handlers.

**Cause**: Dream uses `let%lwt` syntax (ppx extension) for async bindings, which requires the `lwt_ppx` preprocessor.

**Fix**: Added `(preprocess (pps lwt_ppx))` to `lib/dune` build configuration.

---

## Error 4: libev Not Found

**Error**: `libev/ev.h: No such file or directory` during opam package installation.

**Cause**: Dream depends on `conf-libev` which requires the libev C library. On macOS, Homebrew installs it in a non-standard path.

**Fix**: Set environment variables before opam install:
```
C_INCLUDE_PATH="$(brew --prefix libev)/include"
LIBRARY_PATH="$(brew --prefix libev)/lib"
```

---

## Error 5: String.contains_s Does Not Exist in OCaml

**Error**: `Unbound value String.contains_s` in test file.

**Cause**: OCaml's standard library does not have a `String.contains_s` function for substring search.

**Fix**: Replaced with `Str.search_forward (Str.regexp pattern) text 0` from the `str` library. Added `str` to test/dune dependencies.

---

## Error 6: Passive Voice and Adverb Detection Failing

**Error**: Tests for passive voice count and adverb count returned incorrect values.

**Cause**: Words at end of sentences retained trailing punctuation (e.g., `"kicked."` instead of `"kicked"`), preventing suffix-based detection (`"ed"` for passive voice, `"ly"` for adverbs).

**Fix**: Added `strip_punct` helper function to `split_words` that removes trailing `.`, `,`, `;`, `:`, `!`, `?` from each word before analysis.

---

## Error 7: CI OCaml Version Mismatch

**Error**: `No agreement on the version of ocaml: invariant -> 5.2.1, deps -> >= 5.3.0`

**Cause**: CI workflow specified `ocaml-compiler: "5.2"` but the project's opam file requires OCaml >= 5.3.0.

**Fix**: Changed CI workflow to `ocaml-compiler: "5.3"`.

---

## Error 8: Docker Base Image OCaml Version Mismatch

**Error**: Same version conflict as Error 7 but in the Docker build stage.

**Cause**: Dockerfile used `ocaml/opam:debian-12-ocaml-5.2` base image.

**Fix**: Changed to `ocaml/opam:debian-12-ocaml-5.3`.

---

## Error 9: Docker Build - Binary Not Produced

**Error**: `dune build --release` completed in 0.4s without producing `bin/main.exe`.

**Cause**: Dependencies were installed manually with `opam install dream yojson dune --yes` which missed transitive deps. Also, `lwt_ppx` was not installed, and `dune build --release` silently skipped the binary.

**Fix**: Changed to `opam install . --deps-only --yes && opam install lwt_ppx --yes` for proper dependency resolution. Used explicit `opam exec -- dune build bin/main.exe 2>&1` to ensure build errors surface. Added `COPY --chown=opam:opam test/ test/` for complete project structure.

---

## Summary
- Total errors encountered: 9
- All resolved successfully
- Tests passing: 42/42
- CI status: All green (build-and-test + Docker)
