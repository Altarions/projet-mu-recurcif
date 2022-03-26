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
    Successeur
  | Constante    of constante
  | Projection   of projection
  | Substitution of substitution
  | Induction    of induction
  | Minimisation of mu

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


(* ------------------------- variables ------------------------- *)


exception End;;


(* ------------------------- terminal display ------------------------- *)





(* ------------------------- functions ------------------------- *)


let rec arite = function
  | Successeur     -> 1
  | Constante    v -> v.constante_arite
  | Projection   v -> v.projection_arite
  | Substitution v -> arite (List.hd v.substitution_arguments)
  | Induction    v -> arite v.induction_base + 1
  | Minimisation v -> arite v - 1


let rec appliquer = function
  | Successeur     -> appliquer_successeur
  | Constante    v -> appliquer_constante    v
  | Projection   v -> appliquer_projection   v
  | Substitution v -> appliquer_substitution v
  | Induction    v -> appliquer_induction    v
  | Minimisation v -> appliquer_minimisation v

and appliquer_successeur args =
  List.hd args + 1

and appliquer_constante v _ =
  v.constante_valeur

and appliquer_projection v args =
  List.nth args (v.projection_indice - 1)

and appliquer_substitution v args =
  let fonction  = appliquer v.substitution_fonction
  and arguments = List.map appliquer v.substitution_arguments in
  fonction (List.map ((|>) args) arguments)

and appliquer_induction v args =
  let max      = List.hd args
  and args'    = List.tl args
  and base     = appliquer v.induction_base
  and heredite = appliquer v.induction_heredite in
  let rec iterer i acc =
    if i = max then acc
    else iterer (i + 1) (heredite (i::acc::args'))
  in
  iterer 0 (base args')

and appliquer_minimisation v args =
  let predicat = appliquer v in
  let rec rechercher i =
    if predicat (i::args) = 0 then i
    else rechercher (i + 1)
  in
  rechercher 0


let successeur = Successeur

let constante arite valeur =
  if arite < 0 then failwith "constante arite : arite < 0" else
  if valeur < 0 then failwith "constante _ valeur : valeur < 0" else
    Constante {
      constante_arite  = arite;
      constante_valeur = valeur;
    }

let projection arite indice =
  if arite < 0 then failwith "projection arite : arite < 0" else
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

let minimisation predicat = Minimisation predicat


let identite = projection 1 1

let zero = constante 0 0


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
