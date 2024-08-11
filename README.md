# DreamBerd Lite

A lightweight interpreter for the perfect programming language, DreamBerd, written in OCaml.

Because of numerous reasons, including keeping this interpreter light and fast, not all features from [the original DreamBerd](https://github.com/TodePond/DreamBerd) are implemented. Therefore this interpreter is rebranded _DreamBerd Lite_. See the [wiki](https://github.com/olivier-hnry/dblite/wiki) for more about supported syntaxes.

## Installation

To install DreamBerd Lite, you need a working installation of `ocaml`. Use `dune build` to build the interpreter and `dune install` to install it. You can uninstall it using `dune uninstall`.

## Usage

Interpret a program by running `db-lite path/to/program.db` in the conole. To display help, you may use the option `-help` or `--help`.
