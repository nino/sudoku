(lang dune 3.6)

(name sudoku)

(generate_opam_files true)

(source
 (github nino/sudoku))

(authors "Nino Annighoefer")

(maintainers "nino@ninoan.com")

(license LICENSE)

(documentation https://github.com/nino/sudoku)

(package
 (name sudoku)
 (synopsis "Sudoku library")
 (description "Sudoku library written in OCaml that can solve your sudokus and help you write sudoku games.")
 (depends ocaml dune containers ppx_inline_test containers sexplib ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
