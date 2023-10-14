(lang dune 2.8)

(name stt)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github Tchou/stt))

(authors "Kim Nguy\225\187\133n <kim@nguyen.vg>")

(maintainers "Kim Nguy\225\187\133n <kim@nguyen.vg>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stt)
 (synopsis "Set Theoretic Types")
 (description "A library to manipulate Set Theoretic Types in a generic way")
 (depends ocaml dune zarith)
 (tags (types)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
