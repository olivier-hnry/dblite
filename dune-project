(lang dune 3.10)

(name dblite)

(generate_opam_files true)

(source
 (github olivier-hnry/dblite))

(authors "Olivier Henry")

(maintainers "Olivier Henry")

(license The Unlicense)

(documentation https://github.com/olivier-hnry/dblite/wiki)

(package
 (name dblite)
 (synopsis "Light interpreter for DreamBerd")
 (description "A lightweight interpreter for DreamBerd, a perfect programming language, written in OCaml")
 (depends ocaml dune))
