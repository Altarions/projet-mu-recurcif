(*
    @goal:
        //todo

    @authors: 
        - LESENECHAL Adrien (...@etu.univ-nantes.fr)
        - GARNIER Cyprien (cyprien.garnier@etu.univ-nantes.fr)

    @date: 25/03/2021 
*)

(* ------------------------- modules ------------------------- *) 


open Graphics;;
open List;;


(* ------------------------- types ------------------------- *)


(** Fonction mu-récursive *)
type mu = Successeur
        | Constante    of constante
        | Projection   of projection
        | Substitution of substitution
        | Induction    of induction
        | Minimisation of mu

(** Fonction constante à n paramètres produisant c *)
and constante = {n: int; c: int}

(** Fonction à n paramètres projetant le paramètre i *)
and projection = {n: int; i: int}

(** Fonction substituant les paramètres de f par leur application à l *)
and substitution = {f: mu; l: mu list}

(** Fonction d'induction formée sur le cas initial f et le cas d'induction g *)
and induction = {f: mu; g: mu};;


(* ------------------------- variables ------------------------- *)


exception End;;


(* ------------------------- terminal display ------------------------- *)





(* ------------------------- functions ------------------------- *)


(* val appliquer : mu -> int list -> int *)
let rec appliquer mu =
	match mu with
  | Successeur     -> appliquer_successeur
  | Constante    v -> appliquer_constante    v
  | Projection   v -> appliquer_projection   v
  | Substitution v -> appliquer_substitution v
  | Induction    v -> appliquer_induction    v
  | Minimisation v -> appliquer_minimisation v

(* val appliquer_successeur : int list -> int *)
and appliquer_successeur = function
	| n::_ -> n + 1
  | 

(* val appliquer_constante : constante -> int list -> int *)
and appliquer_constante {n; c} l =

(* val appliquer_projection : constante -> int list -> int *)
and appliquer_projection {n; i} l =

(* val appliquer_substitution : substitution -> int list -> int *)
and appliquer_substitution {f; l} l =

(* val appliquer_induction : induction -> int list -> int *)
and appliquer_minimisation mu l =
;;


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
