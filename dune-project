(lang dune 2.8)

(name stt)

(generate_opam_files true)

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
 (depends ocaml dune)
 (tags
  (types)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
