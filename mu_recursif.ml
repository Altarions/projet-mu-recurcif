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
    Successeur    of successeur
  | Constante     of constante
  | Projection    of projection
  | Substitution  of substitution
  | Induction     of induction
  | Minimisation  of minimisation
  | Contradiction of contradiction

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
  minimisation_predicat : mu;
}

and contradiction = {
  contradiction_arite : int;
}

(* Notes :                                                                    *)
(*                                                                            *)
(*  - pour réduire la profondeur des structures générées, les fonctions       *)
(*    successeur appliquées successivement sont combinées en un unique nœud   *)
(*    dont le champ "successeur_puissance" indique le nombre d'applications.  *)
(*  - le nœud "Contradiction" représente une fonction dont la non terminaison *)
(*    est certaine, pour que le simplificateur puisse détecter ces fonctions  *)
(*    et les simplifier correctement.                                         *)


(* ------------------------- variables ------------------------- *)


exception End;;


(* ------------------------- terminal display ------------------------- *)





(* ------------------------- functions ------------------------- *)


let rec arite = function
  | Successeur    _ -> 1
  | Constante     v -> v.constante_arite
  | Projection    v -> v.projection_arite
  | Substitution  v -> arite (List.hd v.substitution_arguments)
  | Induction     v -> arite v.induction_base + 1
  | Minimisation  v -> arite v.minimisation_predicat - 1
  | Contradiction v -> v.contradiction_arite


let rec appliquer mu =
  let f  = appliquer_aux mu
  and ar = arite mu in
  fun args ->
    if List.length args != ar then failwith "appliquer mu args : ar(mu) != #args" else
      f @@ List.map (fun arg -> lazy arg) args

and appliquer_aux = function
  | Successeur    v -> appliquer_successeur    v
  | Constante     v -> appliquer_constante     v
  | Projection    v -> appliquer_projection    v
  | Substitution  v -> appliquer_substitution  v
  | Induction     v -> appliquer_induction     v
  | Minimisation  v -> appliquer_minimisation  v
  | Contradiction v -> appliquer_contradiction v

and appliquer_successeur v args =
  match List.hd args with lazy r -> r + v.successeur_puissance

and appliquer_constante v _ =
  v.constante_valeur

and appliquer_projection v args =
  match List.nth args (v.projection_indice - 1) with lazy r -> r

and appliquer_substitution v =
  let fonction  = appliquer_aux v.substitution_fonction
  and arguments = List.map appliquer_aux v.substitution_arguments in
  fun args -> fonction (List.map (fun g -> lazy (g args)) arguments)

and appliquer_induction v =
  let base     = appliquer_aux v.induction_base
  and heredite = appliquer_aux v.induction_heredite in
  function
  | []                -> failwith "appliquer : erreur interne"
  | lazy max :: args' ->
      let rec iterer i =
        if i < 0 then base args'
        else heredite (Lazy.from_val i :: lazy (iterer (i - 1)) :: args')
      in
      iterer (max - 1)

and appliquer_minimisation v =
  let predicat = appliquer_aux v.minimisation_predicat in
  fun args ->
    let rec rechercher i =
      if predicat (Lazy.from_val i :: args) = 0 then i
      else rechercher (i + 1)
    in
    rechercher 0

and appliquer_contradiction _ =
  let rec yolo args = yolo args in yolo

(* Méthode d'évaluationn :                                                  *)
(*                                                                          *)
(* Les fonctions mu-récursives sont calculées comme des fonctions           *)
(* mathématiques usuelles. Par conséquent, s'il existe un chemin            *)
(* d'évaluation fini d'une fonction, son évaluation doit terminer. Nous     *)
(* souhaitons alors que :                                                   *)
(*  - dans une substitution (f · (g1, ..., ga))(x1, ..., xb), les arguments *)
(*    g1(x1, ..., xb) à ga(x1, ..., xb) ne soient évalués que s'ils sont    *)
(*    nécessaires à l'évaluation de f(...),                                 *)
(*  - dans une induction ρ(f, g)(x1, ..., xb), pour tout i < x1, g(i, ...)  *)
(*    ne soit évalué que s'il est nécessaire à l'évaluation de g(i+1, ...), *)
(*  - dans une minimisation μ(p)(g1, ..., ga), pour tout i, p(i+1, ...) ne  *)
(*    soit évalué que si p(i, ...) > 0. Si l'évaluation de p(i, ...) ne     *)
(*    termine pas, alors nous ne pouvons savoir si μ(p)(...) = i ou non et  *)
(*    cette évaluation ne terminerait donc pas non plus.                    *)
(*                                                                          *)
(* Notes :                                                                  *)
(*                                                                          *)
(*  - pour des questions de performance, nous souhaitons que les fonctions  *)
(*    OCaml puissent être dérivées des structures de données avant          *)
(*    l'application des paramètres. Ainsi, [appliquer f] est une fonction   *)
(*    OCaml dans laquelle tout ce qui est calculable a été calculé.         *)


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
  | Successeur    v -> print_successeur    v
  | Constante     v -> print_constante     v
  | Projection    v -> print_projection    v
  | Substitution  v -> print_substitution  v
  | Induction     v -> print_induction     v
  | Minimisation  v -> print_minimisation  v
  | Contradiction v -> print_contradiction v

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

and print_contradiction v =
  print_string "mu(C[";
  print_int v.contradiction_arite;
  print_string ";1])"


let est_identite l =
  let ar = l |> List.hd |> arite in
  let rec aux i = function
    | []                -> i = ar + 1
    | Projection v :: r -> i = v.projection_indice && i = ar && aux (i + 1) r
    | _                 -> false
  in
  aux 1 l


let substituer_parametre generer indice fonction =
  let arite' = arite fonction in
  if indice <= 0 then failwith "substituer_parametre _ indice : indice ≤ 0" else
  if indice > arite' then failwith "substituer_parametre _ indice fonction : indice > ar(fonction)" else
    let rec construire_arguments acc indice' =
      if indice' = 0 then acc
      else if indice' = indice
      then construire_arguments (generer (arite' - 1) :: acc) (indice' - 1)
      else construire_arguments (Projection {
          projection_arite  = arite' - 1;
          projection_indice = if indice' < indice then indice' else indice' - 1;
        } :: acc) (indice' - 1)
    in
    Substitution {
      substitution_fonction  = fonction;
      substitution_arguments = construire_arguments [] arite';
    }

let parametre_constant valeur =
  substituer_parametre (fun arite -> Constante {
      constante_arite  = arite;
      constante_valeur = valeur;
    })

and parametre_contradiction =
  substituer_parametre (fun arite -> Contradiction {
      contradiction_arite = arite;
    })


let seuil_inlining_induction = ref 100

let modifier_seuil_inlining_induction seuil =
  if seuil < 0 then failwith "modifier_seuil_inlining_induction seuil : seuil < 0" else
    let precedent_seuil = !seuil_inlining_induction in
    seuil_inlining_induction := seuil; precedent_seuil


let rec simplifier mu =
  let rec aux mu_modifiee mu = function
    | s :: r ->
        (match s mu with
         | None     -> aux mu_modifiee mu r
         | Some mu' ->
             print_mu mu';
             print_newline ();
             aux true mu' simplificateurs)
    | _      ->
        if not mu_modifiee then mu else
          match mu with
          | Substitution v -> aux false (Substitution {
              substitution_fonction  = simplifier v.substitution_fonction;
              substitution_arguments = List.map simplifier v.substitution_arguments;
            }) simplificateurs
          | Induction    v -> aux false (Induction {
              induction_base     = simplifier v.induction_base;
              induction_heredite = simplifier v.induction_heredite;
            }) simplificateurs
          | Minimisation v -> aux false (Minimisation {
              minimisation_predicat = simplifier v.minimisation_predicat
            }) simplificateurs
          | _              -> mu
  in
  aux true mu simplificateurs

(* Fonctionnement du simplificateur :                                         *)
(*                                                                            *)
(* L'ensemble des règles de transformation sont appliquées les unes après les *)
(* autres sur le nœud racine. Lorsqu'une règle s'applique, toutes sont        *)
(* retestées jusqu'à ce qu'aucune règle ne puisse être appliquée. Auquel cas, *)
(* le simplificateur descend dans l'arbre lorsque la fonction est composée    *)
(* (par substitution, induction ou minimisation). Une fois les nœuds fils     *)
(* simplifiés, le simplificateur tente de réappliquer les règles de           *)
(* transformation sur la racine. Si elles échouent, il s'arrête, sinon il les *)
(* reteste toutes tant que possible, puis descend dans les nœuds fils à       *)
(* nouveau, et recommence ce même cycle une fois les nœuds fils simplifiés.   *)
(*                                                                            *)
(* Cet algorithme termine et converge si et seulement si le système de        *)
(* réécriture formé par l'ensemble des règles de transformation termine et    *)
(* converge lui-même, sur des fonctions mu-récursives supposées bien formées. *)

and simplificateurs = [
  simplifier_identite;
  simplifier_contradiction;
  simplifier_constante;
  simplifier_projection;
  simplifier_successeur_de_contradiction;
  simplifier_successeur_de_successeur;
  simplifier_successeur_de_constante;
  simplifier_substitution;
  simplifier_induction_sur_contradiction;
  simplifier_induction_sur_constante;
  simplifier_induction_sur_projection;
  simplifier_induction_sur_successeur;
  simplifier_induction_de_contradiction;
  simplifier_induction_de_contradiction_1;
  simplifier_induction_de_constante;
  simplifier_induction_de_constante_1;
  simplifier_minimisation_sur_contradiction;
  simplifier_minimisation_sur_constante;
  simplifier_minimisation_sur_projection;
  simplifier_minimisation_sur_successeur;
  simplifier_minimisation_de_contradiction;
  simplifier_minimisation_de_constante;
]

(* Notes :                                                                *)
(*                                                                        *)
(*  - "simplifier_induction_de_constante" doit s'appliquer avant          *)
(*    "simplifier_induction_de_constante_1" pour qu'une induction ne soit *)
(*    inlinée qu'après ses autres paramètres. Si ce n'est pas fait, les   *)
(*    transformations sur les autres paramètres devront être appliquées   *)
(*    à toute occurence de la fonction d'héritage après inlining.         *)

(**************************************)
(** f · (π[a;1], ..., π[a;a])  -->  f *)
(**************************************)

and simplifier_identite = function
  | Substitution v when est_identite v.substitution_arguments ->
      print_string "simplifier_identite";
      print_newline ();
      Some v.substitution_fonction
  | _ -> None

(******************************)
(** ⊥ · (g1, ..., ga)  -->  ⊥ *)
(******************************)

and simplifier_contradiction = function
  | Substitution {
      substitution_fonction  = Contradiction v;
      substitution_arguments = argument :: _;
    } ->
      print_string "simplifier_constradiction";
      print_newline ();
      Some (Contradiction { contradiction_arite = arite argument })
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

(********************)
(** σ^p · ⊥  -->  ⊥ *)
(********************)

and simplifier_successeur_de_contradiction = function
  | Substitution {
      substitution_fonction  = Successeur _;
      substitution_arguments = Contradiction vc as c :: _;
    } ->
      print_string "simplifier_successeur_de_contradiction";
      print_newline ();
      Some c
  | _ -> None

(**************************************)
(** σ^p · σ^q        -->  σ^(p+q)     *)
(** σ^p · (σ^q · f)  -->  σ^(p+q) · f *)
(**************************************)

and simplifier_successeur_de_successeur = function
  | Substitution {
      substitution_fonction  = Successeur v1;
      substitution_arguments = Successeur v2 :: _;
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
      Some (Constante {
          vc with
          constante_valeur = vc.constante_valeur + vs.successeur_puissance
        } )
  | _ -> None

(************************************************************)
(** f · (g1, ..., ga) · (h1, ..., hb)                       *)
(**                                                         *)
(**  -->  f · (g1 · (h1, ..., hb), ..., ga · (h1, ..., hb)) *)
(************************************************************)

and simplifier_substitution = function
  | Substitution {
      substitution_fonction  = Substitution v;
      substitution_arguments = arguments;
    } ->
      print_string "simplifier_substitution";
      print_newline ();
      Some (Substitution {
          v with
          substitution_arguments = List.map (fun argument -> Substitution {
              substitution_fonction  = argument;
              substitution_arguments = arguments;
            }) v.substitution_arguments
        })
  | _ -> None

(********************)
(** ρ(⊥, ⊥)  -->  ⊥ *)
(********************)

and simplifier_induction_sur_contradiction = function
  | Induction {
      induction_base     = Contradiction v;
      induction_heredite = Contradiction _;
    } ->
      print_string "simplifier_induction_sur_contradiction";
      print_newline ();
      Some (Contradiction { contradiction_arite = v.contradiction_arite + 1 })
  | _ -> None

(***************************************)
(** ρ(C[a-1;v], C[a+1;v])  -->  C[a;v] *)
(***************************************)

and simplifier_induction_sur_constante = function
  | Induction {
      induction_base     = Constante v1;
      induction_heredite = Constante v2;
    } when v1.constante_valeur = v2.constante_valeur ->
      print_string "simplifier_induction_sur_constante";
      print_newline ();
      Some (Constante { v1 with constante_arite = v1.constante_arite + 1 })
  | _ -> None

(*******************************************)
(** ρ(π[a-1;i-1], π[a+1;i+1])  -->  π[a;i] *)
(*******************************************)

and simplifier_induction_sur_projection = function
  | Induction {
      induction_base     = Projection v1;
      induction_heredite = Projection v2;
    } when v1.projection_indice + 2 = v2.projection_indice ->
      print_string "simplifier_induction_sur_projection";
      print_newline ();
      Some (Projection { v1 with projection_indice = v1.projection_indice + 1 })
  | _ -> None

(****************************************)
(** ρ(C[a-1;v], σ · π[a+1;2])  -->  σ^v *)
(****************************************)

and simplifier_induction_sur_successeur = function
  | Induction {
      induction_base     = Constante v;
      induction_heredite = Substitution {
          substitution_fonction  = Successeur { successeur_puissance = 1 };
          substitution_arguments = Projection { projection_indice = 2 } :: _;
        };
    } ->
      print_string "simplifier_induction_sur_successeur";
      print_newline ();
      Some (Successeur { successeur_puissance = v.constante_valeur })
  | _ -> None

(******************************************************************************)
(** ρ(f, g) · (h1, h2, ..., hi-1, ⊥, hi+1, ..., ha)                           *)
(**                                                                           *)
(**  -->  ρ(f · (π[a-2;1], ..., π[a-2;i-2], ⊥, π[a-2;i-1], ..., π[a-2;a-2]),  *)
(**                      g · (π[a;1], ..., π[a;i], ⊥, π[a;i+1], ..., π[a;a])) *)
(**                                      · (h1, h2, ..., hi-1, hi+1, ..., ha) *)
(******************************************************************************)

and simplifier_induction_de_contradiction = function (* TODO *)
  | Substitution {
      substitution_fonction  = Induction vi;
      substitution_arguments = argument :: arguments;
    } ->
      let rec recherche_contradiction indice acc = function
        | Contradiction _ :: arguments' ->
            print_string "simplifier_induction_de_contradiction";
            print_newline ();
            Some (Substitution {
                substitution_fonction  = Induction {
                    induction_base     = parametre_contradiction (indice - 1) vi.induction_base;
                    induction_heredite = parametre_contradiction (indice + 1) vi.induction_heredite;
                  };
                substitution_arguments = List.rev_append acc arguments';
              })
        | argument' :: arguments' ->
            recherche_contradiction (indice + 1) (argument' :: acc) arguments'
        | [] -> None
      in
      recherche_contradiction 2 [argument] arguments
  | _ -> None

(******************************************************************************)
(** ρ(f, g) · (h1, h2, ..., hi-1, C[b;v], hi+1, ..., ha)  -->                 *)
(**                                                                           *)
(** ρ(f · (π[a-2;1], ..., π[a-2;i-2], C[a-2;v], π[a-2;i-1], ..., π[a-2;a-2]), *)
(**                 g · (π[a;1], ..., π[a;i], C[a;v], π[a;i+1], ..., π[a;a])) *)
(**                                      · (h1, h2, ..., hi-1, hi+1, ..., ha) *)
(******************************************************************************)

and simplifier_induction_de_constante = function
  | Substitution {
      substitution_fonction  = Induction vi;
      substitution_arguments = argument :: arguments;
    } ->
      let rec recherche_constante indice acc = function
        | Constante vc :: arguments' ->
            print_string "simplifier_induction_de_constante";
            print_newline ();
            Some (Substitution {
                substitution_fonction  = Induction {
                    induction_base     = parametre_constant vc.constante_valeur (indice - 1) vi.induction_base;
                    induction_heredite = parametre_constant vc.constante_valeur (indice + 1) vi.induction_heredite;
                  };
                substitution_arguments = List.rev_append acc arguments';
              })
        | argument' :: arguments' ->
            recherche_constante (indice + 1) (argument' :: acc) arguments'
        | [] -> None
      in
      recherche_constante 2 [argument] arguments
  | _ -> None

(***************************************)
(** ρ(f, g) · (⊥, h2, ..., ha)  -->  ⊥ *)
(***************************************)

and simplifier_induction_de_contradiction_1 = function
  | Substitution {
      substitution_fonction  = Induction vi;
      substitution_arguments = Contradiction _ as c :: _;
    } ->
      print_string "simplifier_induction_de_contradiction_1";
      print_newline ();
      Some c
  | _ -> None

(******************************************************)
(** ρ(f, g) · (C[b;v], h2, ..., ha)  -->  IND(v) avec *)
(**                                                   *)
(**   IND(0) = f · (h2, ..., ha)                      *)
(**   IND(i) = g · (C[b;i-1], IND(i-1), h2, ..., ha)  *)
(******************************************************)

and simplifier_induction_de_constante_1 = function
  | Substitution {
      substitution_fonction  = Induction vi;
      substitution_arguments = Constante vc :: arguments;
    } when List.length arguments < !seuil_inlining_induction ->
      let rec inliner i acc =
        if i = vc.constante_valeur then acc
        else inliner (i + 1) @@ Substitution {
            substitution_fonction  = vi.induction_heredite;
            substitution_arguments = Constante {
                vc with constante_valeur = i
              } :: acc :: arguments;
          }
      in
      print_string "simplifier_induction_de_constante_1";
      print_newline ();
      Some (inliner 0 @@ Substitution {
          substitution_fonction  = vi.induction_base;
          substitution_arguments = arguments;
        })
  | _ -> None

(*****************)
(** μ(⊥)  -->  ⊥ *)
(*****************)

and simplifier_minimisation_sur_contradiction = function
  | Minimisation {
      minimisation_predicat = Contradiction v;
    } ->
      print_string "simplifier_minimisation_sur_contradiction";
      print_newline ();
      Some (Contradiction { contradiction_arite = v.contradiction_arite - 1 })
  | _ -> None

(***************************************)
(** μ(C[a+1;v])  -->  C[a;0]  si v = 0 *)
(**                   ⊥       sinon    *)
(***************************************)

and simplifier_minimisation_sur_constante = function
  | Minimisation {
      minimisation_predicat = Constante v;
    } ->
      print_string "simplifier_minimisation_sur_constante";
      print_newline ();
      Some (if v.constante_valeur = 0
            then Constante { v with constante_arite = v.constante_arite - 1 }
            else Contradiction { contradiction_arite = v.constante_arite - 1 })
  | _ -> None

(*****************************)
(** μ(π[a+1;1])  -->  C[a;0] *)
(*****************************)

and simplifier_minimisation_sur_projection = function
  | Minimisation {
      minimisation_predicat = Projection ({ projection_indice = 1 } as v);
    } ->
      print_string "simplifier_minimisation_sur_projection";
      print_newline ();
      Some (Constante {
          constante_arite  = v.projection_arite - 1;
          constante_valeur = 0;
        })
  | _ -> None

(*******************)
(** μ(σ^p)  -->  ⊥ *)
(*******************)

and simplifier_minimisation_sur_successeur = function
  | Minimisation {
      minimisation_predicat = Successeur v;
    } ->
      print_string "simplifier_minimisation_sur_successeur";
      print_newline ();
      Some (Contradiction { contradiction_arite = 1 })
  | _ -> None

(******************************************************************************)
(** μ(f) · (h1, ..., hi-1, ⊥, hi+1, ..., ha)                                  *)
(**                                                                           *)
(**  -->  μ(f · (π[a;1], ..., π[a;i], ⊥, π[a;i+1], ..., π[a;a]))              *)
(**                                          · (h1, ..., hi-1, hi+1, ..., ha) *)
(******************************************************************************)

and simplifier_minimisation_de_contradiction = function
  | Substitution {
      substitution_fonction  = Minimisation vm;
      substitution_arguments = arguments;
    } ->
      let rec recherche_constante indice acc = function
        | Contradiction _ :: arguments' ->
            print_string "simplifier_minimisation_de_contradiction";
            print_newline ();
            Some (Substitution {
                substitution_fonction  = parametre_contradiction (indice + 1) vm.minimisation_predicat;
                substitution_arguments = List.rev_append acc arguments';
              })
        | argument :: arguments' ->
            recherche_constante (indice + 1) (argument :: acc) arguments'
        | [] -> None
      in
      recherche_constante 1 [] arguments
  | _ -> None

(******************************************************************************)
(** μ(f) · (h1, ..., hi-1, C[b;v], hi+1, ..., ha)                             *)
(**                                                                           *)
(**  -->  μ(f · (π[a;1], ..., π[a;i], C[a;v], π[a;i+1], ..., π[a;a]))         *)
(**                                          · (h1, ..., hi-1, hi+1, ..., ha) *)
(******************************************************************************)

and simplifier_minimisation_de_constante = function
  | Substitution {
      substitution_fonction  = Minimisation vm;
      substitution_arguments = arguments;
    } ->
      let rec recherche_constante indice acc = function
        | Constante vc :: arguments' ->
            print_string "simplifier_minimisation_de_constante";
            print_newline ();
            Some (Substitution {
                substitution_fonction  = parametre_constant vc.constante_valeur (indice + 1) vm.minimisation_predicat;
                substitution_arguments = List.rev_append acc arguments';
              })
        | argument :: arguments' ->
            recherche_constante (indice + 1) (argument :: acc) arguments'
        | [] -> None
      in
      recherche_constante 1 [] arguments
  | _ -> None


type noeud = Symbole      of char
           | Expression   of noeud list list
           | Substitution of noeud list * noeud list


let string_of_noeud_list liste =
  let buffer = Buffer.create (List.length liste) in
  let rec aux = function
    | Symbole s -> Buffer.add_char buffer s
    | _         -> failwith "string_of_noeud_list : fonction mal construite"
  in
  List.iter aux liste;
  Buffer.contents buffer


let int_of_noeud_list liste = int_of_string @@ string_of_noeud_list liste


let analyser_mu liste =
  let rec aux = function
    | [Symbole 'S'] -> successeur
    | [Symbole 'P'; Expression [arite; indice]] ->
        projection (int_of_noeud_list arite) (int_of_noeud_list indice)
    | [Symbole 'C'; Expression [arite; valeur]] ->
        projection (int_of_noeud_list arite) (int_of_noeud_list valeur)
    | [Substitution (fonction, [Expression arguments])] ->
        substitution (aux fonction) (List.map aux arguments)
    | [Substitution (fonction, argument)] ->
        substitution (aux fonction) [aux argument]
    | [Symbole 'R'; Expression [base; heredite]] ->
        induction (aux base) (aux heredite)
    | [Symbole 'M'; Expression [predicat]] -> minimisation(aux predicat)
    | [Expression [fonction]] -> aux fonction
    | liste ->
        let definition = string_of_noeud_list liste in
        failwith "TODO : accès définition"
  in
  aux liste


(* ------------------------- main ------------------------- *)


let string_to_mu chaine =
  let max = String.length chaine in
  let rec loop i =
    if i < max then
      match (String.get chaine i) with 
        | 
      
  in loop 0 
;;


let main () =

;;


(* ------------------------- execution ------------------------- *)


main ();;
