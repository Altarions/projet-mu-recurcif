(lang dune 3.0)

(name projet_mu_recursif)

(generate_opam_files true)

(source
 (github Altarions/projet-mu-recursif))

(authors
 "GARNIER Cyprien <cyprien.garnier@etu.univ-nantes.fr>"
 "LESENECHAL Adrien <adrien.lesenechal@etu.univ-nantes.fr>")

(package
 (name projet_mu_recursif)
 (description "OCaml implementation of general recursive functions.")
 (depends ocaml dune))
