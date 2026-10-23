(lang dune 3.0)

(using menhir 2.1)

(name parkour)

(generate_opam_files true)

(license MIT)

(package
 (name parkour)
 (synopsis
  "Parser combinators with excellent error messages and error recovery")
 (depends
  (ocaml
   (>= 4.14))
  (qcheck-core
   (and
    :with-test
    (>= 0.20)))))
