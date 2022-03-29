(*
    @goal:
        //todo

    @authors: 
        - LESENECHAL Adrien (adrien.lesenechal@etu.univ-nantes.fr)
        - GARNIER Cyprien (cyprien.garnier@etu.univ-nantes.fr)

    @date: 25/03/2021 
*)

(* ------------------------- modules ------------------------- *) 


open Graphics;;
open List;;


(* ------------------------- types ------------------------- *)


type mu =
    Successeur   of successeur
  | Constante    of constante
  | Projection   of projection
  | Substitution of substitution
  | Induction    of induction
  | Minimisation of minimisation

and successeur = {
  successeur_puissance : int;
}

and constante = {
  constante_arite  : int;
  constante_valeur : int;
}

and projection = {
  projection_arite  : int;
  projection_indice : int;
}

and substitution = {
  substitution_fonction  : mu;
  substitution_arguments : mu list;
}

and induction = {
  induction_base     : mu;
  induction_heredite : mu;
}

and minimisation = {
  minimisation_predicat: mu;
}


(* ------------------------- variables ------------------------- *)


exception End;;


(* ------------------------- terminal display ------------------------- *)





(* ------------------------- functions ------------------------- *)


let rec arite = function
  | Successeur   _ -> 1
  | Constante    v -> v.constante_arite
  | Projection   v -> v.projection_arite
  | Substitution v -> arite (List.hd v.substitution_arguments)
  | Induction    v -> arite v.induction_base + 1
  | Minimisation v -> arite v.minimisation_predicat - 1


  let rec appliquer = function
  | Successeur   v -> appliquer_successeur   v
  | Constante    v -> appliquer_constante    v
  | Projection   v -> appliquer_projection   v
  | Substitution v -> appliquer_substitution v
  | Induction    v -> appliquer_induction    v
  | Minimisation v -> appliquer_minimisation v

and appliquer_successeur v args =
  List.hd args + v.successeur_puissance

and appliquer_constante v _ =
  v.constante_valeur

and appliquer_projection v args =
  List.nth args (v.projection_indice - 1)

and appliquer_substitution v =
  let fonction  = appliquer v.substitution_fonction
  and arguments = List.map appliquer v.substitution_arguments in
  fun args -> fonction (List.map ((|>) args) arguments)

and appliquer_induction v =
  let base     = appliquer v.induction_base
  and heredite = appliquer v.induction_heredite in
  function
  | []           -> failwith "appliquer_induction _ args : args = []"
  | max :: args' ->
      let rec iterer i acc =
        if i = max then acc
        else iterer (i + 1) (heredite (i::acc::args'))
      in
      iterer 0 (base args')

and appliquer_minimisation v =
  let predicat = appliquer v.minimisation_predicat in
  fun args ->
    let rec rechercher i =
      if predicat (i::args) = 0 then i
      else rechercher (i + 1)
    in
    rechercher 0


let successeur =
  Successeur {
    successeur_puissance = 1
  }

let constante arite valeur =
  if arite < 0 then failwith "constante arite : arite < 0" else
  if valeur < 0 then failwith "constante _ valeur : valeur < 0" else
    Constante {
      constante_arite  = arite;
      constante_valeur = valeur;
    }

let projection arite indice =
  if indice <= 0 then failwith "projection _ indice : indice ≤ 0" else
  if indice > arite then failwith "projection arite indice : indice > arite" else
    Projection {
      projection_arite  = arite;
      projection_indice = indice;
    }

let substitution fonction = function
  | []                -> failwith "substitution _ arguments : arguments = []"
  | f::r as arguments ->
      let n = arite f in
      if arite fonction != List.length arguments then failwith "substitution fonction arguments : ar(fonction) != #arguments" else
      if List.exists (fun f -> arite f != n) r then failwith "substitution _ [fi, ..., fj, ...] : ar(fi) != ar(fj)" else
        Substitution {
          substitution_fonction  = fonction;
          substitution_arguments = arguments;
        }

let induction base heredite =
  if arite base + 2 != arite heredite then failwith "induction base heredite : ar(base) + 2 != ar(heredite)" else
    Induction {
      induction_base     = base;
      induction_heredite = heredite;
    }

let minimisation predicat =
  Minimisation {
    minimisation_predicat = predicat;
  }


let identite = projection 1 1

let zero = constante 0 0


let rec print_mu = function
  | Successeur   v -> print_successeur   v
  | Constante    v -> print_constante    v
  | Projection   v -> print_projection   v
  | Substitution v -> print_substitution v
  | Induction    v -> print_induction    v
  | Minimisation v -> print_minimisation v

and print_successeur v =
  print_string "S";
  for i = 2 to v.successeur_puissance do
    print_string " . S"
  done

and print_constante v =
  print_string "C[";
  print_int v.constante_arite;
  print_string ";";
  print_int v.constante_valeur;
  print_string "]"

and print_projection v =
  print_string "P[";
  print_int v.projection_arite;
  print_string ";";
  print_int v.projection_indice;
  print_string "]"

and print_substitution v =
  print_mu v.substitution_fonction;
  print_string " . ";
  begin
    match v.substitution_arguments with
    |                  []         -> failwith "print_mu mu : mu = f.()"
    | Substitution v'::[]         ->
        print_string "(";
        print_substitution v';
        print_string ")"
    | argument       ::[]         ->
        print_mu argument
    | argument       ::arguments' ->
        print_string "(";
        print_mu argument;
        List.iter (fun argument -> print_string ", "; print_mu argument) arguments';
        print_string ")"
  end

and print_induction v =
  print_string "p(";
  print_mu v.induction_base;
  print_string ", ";
  print_mu v.induction_heredite;
  print_string ")"

and print_minimisation v =
  print_string "mu(";
  print_mu v.minimisation_predicat;
  print_string ")"


let est_identite l =
  let ar = l |> List.hd |> arite in
  let rec aux i = function
    | []                -> i = ar + 1
    | Projection v :: r -> i = v.projection_indice && i = ar && aux (i + 1) r
    | _                 -> false
  in
  aux 1 l


let nb_passes_simplificateur = ref 2
let seuil_inlining_induction = ref 100


let rec simplifier mu =
  let rec aux passe mu l =
    if passe > !nb_passes_simplificateur then mu else
      match l with
      | s :: r ->
          (match s mu with
           | None     -> aux passe mu r
           | Some mu' -> print_mu mu';print_newline (); simplifier mu')
      | _      ->
          match mu with
          | Substitution v -> aux (passe + 1) (Substitution {
              substitution_fonction  = simplifier v.substitution_fonction;
              substitution_arguments = List.map simplifier v.substitution_arguments;
            }) simplificateurs
          | Induction    v -> aux (passe + 1) (Induction {
              induction_base     = simplifier v.induction_base;
              induction_heredite = simplifier v.induction_heredite;
            }) simplificateurs
          | Minimisation v -> aux (passe + 1) (Minimisation {
              minimisation_predicat = simplifier v.minimisation_predicat
            }) simplificateurs
          | _              -> mu
  in
  aux 1 mu simplificateurs

and simplificateurs = [
  simplifier_successeur_de_successeur;
  simplifier_indentite;
  simplifier_constante;
  simplifier_projection;
  simplifier_substitution;
  simplifier_successeur_de_constante;
  simplifier_induction_de_constante;
]

(**************************************)
(** σ^p · σ^q        -->  σ^(p+q)     *)
(** σ^p · (σ^q · f)  -->  σ^(p+q) · f *)
(**************************************)

and simplifier_successeur_de_successeur = function
  | Substitution {
      substitution_fonction  = Successeur v1;
      substitution_arguments = Successeur v2 :: [];
    } ->
      print_string "simplifier_successeur_de_successeur";
      print_newline ();
      Some (Successeur { successeur_puissance = v1.successeur_puissance + v2.successeur_puissance })
  | Substitution {
      substitution_fonction  = Successeur v1;
      substitution_arguments = Substitution {
          substitution_fonction  = Successeur v2;
          substitution_arguments = arguments;
        } :: [];
    } ->
      print_string "simplifier_successeur_de_successeur";
      print_newline ();
      Some (Substitution {
          substitution_fonction  = Successeur { successeur_puissance = v1.successeur_puissance + v2.successeur_puissance };
          substitution_arguments = arguments;
        })
  | _ -> None

(**************************************)
(** f · (π[a;1], ..., π[a;a])  -->  f *)
(**************************************)

and simplifier_indentite = function
  | Substitution v when est_identite v.substitution_arguments ->
      print_string "simplifier_indentite";
      print_newline ();
      Some v.substitution_fonction
  | _ -> None

(*********************************************)
(** C[a;v] · (g1, ..., ga)  -->  C[ar(g1);v] *)
(*********************************************)

and simplifier_constante = function
  | Substitution {
      substitution_fonction  = Constante v;
      substitution_arguments = argument :: _;
    } ->
      print_string "simplifier_constante";
      print_newline ();
      Some (Constante { v with constante_arite = arite argument })
  | _ -> None

(************************************)
(** π[a;i] · (g1, ..., ga)  -->  gi *)
(************************************)

and simplifier_projection = function
  | Substitution {
      substitution_fonction  = Projection v;
      substitution_arguments = arguments;
    } ->
      print_string "simplifier_projection";
      print_newline ();
      Some (List.nth arguments (v.projection_indice - 1))
  | _ -> None

(**********************************************************************************************)
(** f · (g1, ..., ga) · (h1, ..., hb)  -->  f · (g1 · (h1, ..., hb), ..., ga · (h1, ..., hb)) *)
(**********************************************************************************************)

and simplifier_substitution = function
  | Substitution {
      substitution_fonction  = Substitution v;
      substitution_arguments = arguments;
    } ->
      print_string "simplifier_substitution";
      print_newline ();
      Some (Substitution { v with substitution_arguments = List.map (Fun.flip substitution arguments) v.substitution_arguments })
  | _ -> None

(********************************)
(** σ^p · C[a;v]  -->  C[a;v+p] *)
(********************************)

and simplifier_successeur_de_constante = function
  | Substitution {
      substitution_fonction  = Successeur vs;
      substitution_arguments = Constante vc :: _;
    } ->
      print_string "simplifier_successeur_de_constante";
      print_newline ();
      Some (Constante { vc with constante_valeur = vc.constante_valeur + vs.successeur_puissance } )
  | _ -> None

(******************************************************)
(** ρ(f, g) · (C[b;v], h2, ..., ha)  -->  IND(v) avec *)
(**                                                   *)
(**   IND(0) = f · (h2, ..., ha)                      *)
(**   IND(i) = g · (C[b;i-1], IND(i-1), h2, ..., ha)  *)
(******************************************************)

and simplifier_induction_de_constante = function
  | Substitution {
      substitution_fonction  = Induction vi;
      substitution_arguments = Constante vc :: arguments;
    } when List.length arguments < !seuil_inlining_induction ->
      let rec aux i acc =
        if i = vc.constante_valeur then acc
        else aux (i + 1) @@ Substitution {
            substitution_fonction  = vi.induction_heredite;
            substitution_arguments = Constante { vc with constante_valeur = i }::acc::arguments;
          }
      in
      print_string "simplifier_induction_de_constante";print_newline (); Some (aux 0 @@ Substitution {
          substitution_fonction  = vi.induction_base;
          substitution_arguments = arguments;
        })
  | _ -> None


(* ------------------------- discussion ------------------------- *)


(* string_to_mu elle doit retourner le type mu ?*)

(**
On peut peut-être le faire en plusieurs étapes, puisqu'on devra notamment vérifier que les nombre de paramètres lors de compositions est bon, etc.
Du coup, par exemple, on peut commencer par :
 - transformer la chaîne de caractères en une liste de symboles, en retirant les espaces ou autres,
 - transformer cette liste en arbre, en traitant les appels de fonctions et compositions, i.e.  S . mu(a, b) donne quelque chose comme Noeud(Comp, S, Noeud(Mu, a, b))
 - puis transformer l'arbre en mu en vérifiant les paramètres, et autres choses...
	ok je comprends !


LA PROF EST PASSÉE.
 et ?

Donc :
	- les "optimisations" de fonctions (réductions ou autre) c'est vraiment optionnel,
  - la conversion d'une chaîne de caractères, on risque de galérer lorsqu'il va falloir traiter le parenthésage et la composition (passer d'une liste de symboles en arbre quoi).
ok du coup elle propose un truc ou on doit trouver ?

Elle dit qu'il existe des librairies standard de "parsing", mais que sans les utiliser, on peut se simplifier la vie en ne considérant que des parenthèses,
donc pour "S . p(a, b)" on aurait ".(S, p(a, b))" ou quelque chose de similaire.
ok du coup les ( ) découpe chaque expression
Détecter à quel endroit est le point est plus compliqué d'après elle, elle n'a pas dit que ce n'était pas faisable, juste plus dur, donc c'est au choix.
On peut commencer à ne gérer que des parenthèses et améliorer après sinon.
Après elle s'étonne que tout le monde choisissent les automates cellulaires ou réseaux de pétri x)
je vais faire de mon mieux pour gerer la traduction !
Bah bonne chance. x) je fais la fonction (ci-dessus) pour "appliquer" une fonction mu-récursive à des paramètres si ça te va.
ya


Y a TD, du coup je vais bouger.
On a TD de CEL à 17h après, donc je ne pourrai bosser sur l'info fonda qu'à partir de 6h30.
olala mais l'emploie du temps throw vraiment en ce moment
tkt je vais faire la partie liaison avec le cours

x)
J'ai rédigé l'exemple tout-à-l'heure, n'hésites surtout pas à réécrire quoi que ce soit si c'est pas assez compréhensible.
j'ai vu tkt

Bah à tout-à-l'heure.
yep

*)

(* ------------------------- main ------------------------- *)


let string_to_mu chaine =
		let size = String.length chaine in

    let rec orienteur chaine acc =
      match acc with
        | i when i = size -> ""
        | _ -> chaine.[acc]; orienteur chaine acc+1;
    in orienteur chaine 0
;;


let main () =

;;


(* ------------------------- execution ------------------------- *)


main ();;
