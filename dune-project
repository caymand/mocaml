(lang dune 3.15)

(name mocaml)

(generate_opam_files true)

(source
 (github caymand/mocaml))

(authors "Kristoffer Kortbaek")

(maintainers "Kristoffer Kortbaek")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
