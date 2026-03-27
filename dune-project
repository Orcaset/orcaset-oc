(lang dune 3.0)

(name orcaset)

(generate_opam_files true)

(source
 (github Orcaset/orcaset-oc))

(authors "Orcaset")

(maintainers "Orcaset")
(license SSPL-1.0)

(package
 (name orcaset)
 (synopsis "Financial models in OCaml")
 (description "Build strongly-type, composable financial models using OCaml")
 (depends
   (ocaml (>= 5.4.0))

  ;; Dev dependencies
  (ocamlformat
   (and
    (>= 0.29.0)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)
  
  ))
