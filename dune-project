(lang dune 3.6)

(name tabr)

(generate_opam_files true)

(source
 (github BranislavLazic/tabr))

(authors "Branislav Lazic")

(maintainers "Branislav Lazic")

(license MIT)

(package
 (name tabr)
 (allow_empty)
 (description "Functional utilities")
 (depends ocaml dune (ocamlformat (>= 0.26.0)) (alcotest (>= 1.7.0)))
 (tags
  (topics "Functional utilities" "Branislav Lazic" "tabr")))

