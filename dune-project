(lang dune 3.7)
(using menhir 2.0)

(name stt)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github Tchou/stt))

(authors "Kim Nguyễn <kim@nguyen.vg>")

(maintainers "Kim Nguyễn <kim@nguyen.vg>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stt)
 (synopsis "Set Theoretic Types")
 (description "A library to manipulate Set Theoretic Types in a generic way")
 (depends ocaml dune zarith menhir menhirLib sedlex)
 (tags (types)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
