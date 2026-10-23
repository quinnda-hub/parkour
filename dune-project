(lang dune 3.0)

(using menhir 2.1)

(name parkour)

(generate_opam_files true)

(license MIT)

(authors "Quinn <quinn@quinnanhorn.com>")

(maintainers "Quinn <quinn@quinnanhorn.com>")

(package
 (name parkour)
 (synopsis
  "Parser combinators with excellent error messages and error recovery")
 (description
  "Parkour is a parser-combinator library for human-authored text such as configuration files, DSLs and small languages. It reports precise, well-rendered syntax errors with structured diagnostics, offers opt-in error recovery that reports many problems in one run, works on strings or token streams, and limits recursion depth to survive hostile input. It has no runtime dependencies beyond the OCaml standard library.")
 (tags
  (parser "parser combinators" "error recovery" dsl))
 (depends
  (ocaml
   (>= 4.14))
  (qcheck-core
   (and
    :with-test
    (>= 0.20)))))
