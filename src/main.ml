module Dictionnaire = Map.Make(String)

type 'a dictionnaire = 'a Dictionnaire.t

type noeud = Symbole      of char
           | Expression   of noeud list list
           | Substitution of noeud list * noeud list


let concatener_symboles liste =
  let buffer = Buffer.create (List.length liste) in
  let rec parcourir = function
    | Symbole s :: r -> Buffer.add_char buffer s; parcourir r
    | liste'         -> Buffer.contents buffer, liste'
  in
  parcourir liste


let string_of_noeud_list liste =
  let buffer = Buffer.create (2 * List.length liste) in
  let aux = function
    | Symbole s -> Buffer.add_char buffer s
    | _         -> Buffer.add_char buffer '?'; Buffer.add_char buffer '?'
  in
  List.iter aux liste;
  Buffer.contents buffer


let int_of_noeud_list liste =
  let chaine = string_of_noeud_list liste in
  match int_of_string_opt chaine with
  | Some i -> i
  | None   -> failwith @@ Printf.sprintf "Entier attendu, '%s' obtenu." chaine


let noeud_list_of_string chaine = failwith "TODO"; [Symbole 'S']


let mu_of_noeud_list dictionnaire liste =
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
        let nom, reste = concatener_symboles liste in
        if reste <> [] then failwith "Expression invalide" else
          match Dictionnaire.find_opt nom dictionnaire with
          | Some mu -> mu
          | None    -> failwith @@ Printf.sprintf "Fonction '%s' indefinie." nom
  in
  aux liste


let mu_of_string dictionnaire chaine =
  mu_of_noeud_list dictionnaire @@ noeud_list_of_string chaine


(* ------------------------- main ------------------------- *)


(*let string_to_mu chaine =
  let max = String.length chaine in
  let rec loop i =
    if i < max then
      match (String.get chaine i) with 
        | 
      
  in loop 0 
;;*)


let est_nom_valide = String.for_all (fun c ->
    c = '_' ||
    'a' <= c && c <= 'z' ||
    'A' <= c && c <= 'Z' ||
    '0' <= c && c <= '9'
  )


let definir_fonction dictionnaire nom definition =
  if not (est_nom_valide nom) then failwith "Nom de fonction invalide" else
    Dictionnaire.add nom (mu_of_string dictionnaire definition) dictionnaire


let analyser_definition dictionnaire ligne =
  match String.split_on_char '=' ligne with
  | nom :: definition :: [] -> definir_fonction dictionnaire nom definition
  | _   :: _          :: _  -> failwith "Symbole = en trop"
  | _   :: _                -> failwith "Symbole = absent"
  | _                       -> failwith "Erreur interne"


let charger_fichier dictionnaire ligne =
  let canal = open_in @@ String.sub ligne 1 (String.length ligne) in
  let rec lire_ligne dictionnaire =
    try let ligne = String.trim @@ input_line canal in
      lire_ligne (
        if ligne = "" || String.starts_with ~prefix:"#" ligne then dictionnaire
        else analyser_definition dictionnaire ligne
      )
    with End_of_file -> close_in canal; dictionnaire
  in
  lire_ligne dictionnaire


let resoudre_application dictionnaire application =
  match concatener_symboles @@ noeud_list_of_string application dictionnaire with
  | nom, [Expression arguments] ->
    Mu.appliquer (
      match Dictionnaire.find_opt nom dictionnaire with
      | Some mu -> mu
      | None    -> failwith @@ Printf.sprintf "Fonction '%s' indefinie."
    ) @@ List.map int_of_noeud_list arguments
  | _ -> failwith "Application invalide."


let rec attendre_saisie dictionnaire =
  let ligne = String.trim (read_line ()) in (
    if String.starts_with ~prefix: "@" ligne then charger_fichier
    else if String.contains ligne '=' then analyser_definition
    else resoudre_application
  ) dictionnaire ligne


let () =
  let _ = attendre_saisie Dictionnaire.empty; in ()