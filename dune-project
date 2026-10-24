(lang dune 3.5)

(using menhir 2.1)

(name parkour)

(generate_opam_files true)

(source
 (github quinnda-hub/parkour))

(license MIT)

(authors "Quinn Anhorn <quinn@quinnanhorn.com>")

(maintainers "Quinn Anhorn <quinn@quinnanhorn.com>")

(package
 (name parkour)
 (synopsis
  "Parser combinators with excellent error messages and error recovery")
 (description
  "Parkour is a parser-combinator library for human-authored text such as configuration files, DSLs and small languages. It reports precise, well-rendered syntax errors with structured diagnostics, offers opt-in error recovery that reports many problems in one run, works on strings or token streams, and is stack-safe on long and deeply nested input (nesting depth is limited). It has no runtime dependencies beyond the OCaml standard library.")
 (tags
  (parser "parser combinators" "error recovery" dsl))
 (depends
  (ocaml
   (>= 4.14))
  (qcheck-core
   (and
    :with-test
    (>= 0.90)))))
