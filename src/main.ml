type noeud = Symbole      of char
           | Expression   of noeud list list
           | Substitution of noeud list * noeud list


let string_of_noeud_list liste =
  let buffer = Buffer.create (List.length liste) in
  let aux = function
    | Symbole s -> Buffer.add_char buffer s
    | _         -> failwith "string_of_noeud_list : fonction mal construite"
  in
  List.iter aux liste;
  Buffer.contents buffer


let int_of_noeud_list liste = int_of_string @@ string_of_noeud_list liste


let analyser_mu liste =
  let rec aux = function
    | [Symbole 'S'] -> Mu.successeur
    | [Symbole 'P'; Expression [arite; indice]] ->
        Mu.projection (int_of_noeud_list arite) (int_of_noeud_list indice)
    | [Symbole 'C'; Expression [arite; valeur]] ->
        Mu.projection (int_of_noeud_list arite) (int_of_noeud_list valeur)
    | [Substitution (fonction, [Expression arguments])] ->
        Mu.substitution (aux fonction) (List.map aux arguments)
    | [Substitution (fonction, argument)] ->
        Mu.substitution (aux fonction) [aux argument]
    | [Symbole 'R'; Expression [base; heredite]] ->
        Mu.induction (aux base) (aux heredite)
    | [Symbole 'M'; Expression [predicat]] -> Mu.minimisation(aux predicat)
    | [Expression [fonction]] -> aux fonction
    | liste ->
        let definition = string_of_noeud_list liste in
        failwith "TODO : accès définition"
  in
  aux liste


(* ------------------------- main ------------------------- *)


(*let string_to_mu chaine =
  let max = String.length chaine in
  let rec loop i =
    if i < max then
      match (String.get chaine i) with 
        | 
      
  in loop 0 
;;*)



module Dictionnaire = Map.Make(String)

type 'a dictionnaire = 'a Dictionnaire.t


let mu_of_string chaine dictionnaire = Dictionnaire.find "" @@ Dictionnaire.add chaine Mu.successeur dictionnaire


let est_nom_valide = String.for_all (fun c ->
    c = '_' ||
    'a' <= c && c <= 'z' ||
    'A' <= c && c <= 'Z' ||
    '0' <= c && c <= '9'
  )


let definir_fonction nom definition dictionnaire =
  if not (est_nom_valide nom) then failwith "definir_fonction : nom invalide" else
    Dictionnaire.add nom (mu_of_string definition dictionnaire) dictionnaire


let analyser_definition ligne =
  match String.split_on_char '=' ligne with
  | nom :: definition :: [] -> definir_fonction nom definition
  | _   :: _          :: _  -> failwith "attendre_saisie : symbole = en trop"
  | _   :: _                -> failwith "attendre_saisie : symbole = absent"
  | _                       -> failwith "attendre_saisie : erreur interne"


let charger_fichier ligne =
  let canal = open_in @@ String.sub ligne 1 (String.length ligne) in
  let rec lire_ligne dictionnaire =
    try let ligne = String.trim @@ input_line canal in
      lire_ligne (
        if ligne = "" || String.starts_with ~prefix:"#" ligne then dictionnaire
        else analyser_definition ligne dictionnaire
      )
    with End_of_file -> close_in canal; dictionnaire
  in
  lire_ligne


let resoudre_application application =
  failwith "TODO : nom(...)"


let rec attendre_saisie =
  let ligne = String.trim (read_line ()) in (
    if String.starts_with ~prefix: "@" ligne then charger_fichier
    else if String.contains ligne '=' then analyser_definition
    else resoudre_application
  ) ligne


let () =
  let _ = attendre_saisie Dictionnaire.empty; in ()