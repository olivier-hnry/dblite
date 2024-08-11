(lang dune 3.10)

(name db-lite)

(generate_opam_files true)

(source
 (github olivier-hnry/dblite))

(authors "Olivier Henry")

(maintainers "Olivier Henry")

(license The Unlicense)

(documentation https://github.com/olivier-hnry/dblite/wiki)

(package
 (name db-lite)
 (synopsis "Light interpreter for DreamBerd")
 (description "A lightweight interpreter for the perfect programming language, DreamBerd, written in OCaml")
 (depends ocaml dune))
