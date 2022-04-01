module Dictionnaire = Map.Make(String)

type noeud = Symbole      of char
           | Expression   of noeud list
           | Expression'  of noeud list list
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


let erreur_parenthese chaine =
  let ouvrant = ref 0 in
  let fermant = ref 0 in
  let loop i =
    match (i) with
    | '(' -> (ouvrant := !ouvrant+1);
    | ')' -> (fermant := !fermant+1); 
    | _   -> ()
  in String.iter (loop) chaine;       
  !ouvrant <> !fermant


let traiter_virgules liste =
  let rec folder noeud acc =
    match noeud, acc with
    | Symbole    ',', acc    -> [] :: acc
    | Expression e  , l :: r -> (Expression' (traiter e) :: l) :: r
    | _             , l :: r -> (noeud :: l) :: r
    | _                      -> failwith "Erreur interne"
  and traiter liste =
    List.fold_right folder liste [[]]
  in
  match traiter liste with
  | [ok] -> ok
  | _    -> failwith "Virgule a la racine"


let rec traiter_substitutions liste =
  let folder noeud acc =
    match noeud, acc with
    | Symbole     '.', acc    -> [] :: acc
    | Expression' e  , l :: r -> (Expression' (List.map traiter_substitutions e) :: l) :: r
    | _              , l :: r -> (noeud :: l) :: r
    | _                       -> failwith "Erreur interne"
  in
  match List.fold_right folder liste [[]] with
  | acc :: l -> List.fold_left (fun acc l -> [Substitution (acc, l)]) acc l
  | _        -> failwith "Erreur interne"


let noeud_list_of_string chaine =
  let rec aux chaine =
    let rec loop i list =
      if i < String.length chaine then
        match (String.get chaine i) with
        | '(' -> let list',i'= aux (String.sub chaine (i+1) ((String.length chaine)-(i+1))) in 
            loop (i+i'+2) (list@[Expression (list')])  
        | ')' -> (list,i)
        | ' ' -> loop (i+1) (list)
        | a -> loop (i+1) (list@[Symbole a])
      else list,i 
    in loop 0 []
  in traiter_substitutions @@ traiter_virgules @@ fst (aux chaine)


let mu_of_noeud_list dictionnaire liste =
  let rec aux = function
    | [Symbole 'S'] -> Mu.successeur
    | [Symbole 'P'; Expression' [arite; indice]] ->
        Mu.projection (int_of_noeud_list arite) (int_of_noeud_list indice)
    | [Symbole 'C'; Expression' [arite; valeur]] ->
        Mu.constante (int_of_noeud_list arite) (int_of_noeud_list valeur)
    | [Substitution (fonction, [Expression' arguments])] ->
        Mu.substitution (aux fonction) (List.map aux arguments)
    | [Substitution (fonction, argument)] ->
        Mu.substitution (aux fonction) [aux argument]
    | [Symbole 'R'; Expression' [base; heredite]] ->
        Mu.induction (aux base) (aux heredite)
    | [Symbole 'M'; Expression' [predicat]] -> Mu.minimisation(aux predicat)
    | [Expression' [fonction]] -> aux fonction
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
  if erreur_parenthese definition then failwith "Erreur de parenthesage" else
    let mu = Mu.simplifier @@ mu_of_string dictionnaire definition in
    print_string nom;
    print_string " = ";
    Mu.print_mu mu;
    print_newline ();
    Dictionnaire.add nom mu dictionnaire


let analyser_definition dictionnaire ligne =
  match String.split_on_char '=' ligne with
  | nom :: definition :: [] -> definir_fonction dictionnaire (String.trim nom) definition
  | _   :: _          :: _  -> failwith "Symbole = en trop"
  | _   :: _                -> failwith "Symbole = absent"
  | _                       -> failwith "Erreur interne"


let charger_fichier dictionnaire ligne =
  let canal = open_in @@ String.sub ligne 1 (String.length ligne - 1) in
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
  match concatener_symboles @@ noeud_list_of_string application with
  | nom, [Expression' arguments] ->
      print_int @@ Mu.appliquer (
        match Dictionnaire.find_opt nom dictionnaire with
        | Some mu -> mu
        | None    -> failwith @@ Printf.sprintf "Fonction '%s' indefinie." nom
      ) @@ List.map int_of_noeud_list arguments;
      dictionnaire
  | _ -> failwith "Application invalide."


let rec attendre_saisie dictionnaire =
  try
    let ligne = String.trim (read_line ()) in
    if ligne = "" then attendre_saisie dictionnaire
    else attendre_saisie @@ (
      if String.starts_with ~prefix: "@" ligne then charger_fichier
      else if String.contains ligne '=' then analyser_definition
      else resoudre_application
    ) dictionnaire ligne
  with Failure message ->
    print_string "ERREUR : ";
    print_endline message;
    attendre_saisie dictionnaire


let () =
  print_endline "INSTRUCTIONS :";
  print_endline "\t- \"MaFonction = ...\" enregistre la fonction \"MaFonction\" en memoire. Si celle-ci existait deja, elle est remplacee.";
  print_endline "\t- \"MaFonction(1,2,3)\" applique la fonction precedemment declaree \"MaFonction\" aux parametres 1, 2 et 3.";
  print_endline "\t- \"@mon_fichier\" charge en memoire les definitions de fonctions renseignees dans le fichier \"mon_fichier\", en omettant les lignes commencant par un \"#\".";
  print_endline "";
  print_endline "FORMAT :";
  print_endline "\t- \"S\" est la fonction successeur,";
  print_endline "\t- \"P(a,i)\" est la fonction de projection d'arite \"a\" et projetant le parametre d'indice \"i\",";
  print_endline "\t- \"C(a,v)\" est la fonction constante d'arite \"a\" et renvoyant \"v\",";
  print_endline "\t- \"f . g\" est la substitution de l'unique parametre de \"f\" par l'application de \"g\",";
  print_endline "\t- \"f . (g, h, k)\" est la substitution des parametres de \"f\" par l'application des fonctions \"g\", \"h\" et \"k\",";
  print_endline "\t- \"R(b, h)\" est la fonction d'induction de cas de base \"b\" et d'heredite \"h\",";
  print_endline "\t- \"M(p)\" est la fonction de minimisation du predicat \"p\", et";
  print_endline "\t- \"(f)\" est equivalent a \"f\".";
  print_endline "";
  print_endline "(note : les espaces n'ont jamais d'importance)";
  print_endline "";
  let _ = attendre_saisie Dictionnaire.empty; in ()
