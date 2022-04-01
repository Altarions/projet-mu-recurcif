type t
(** Le type des fonctions mu-récursives *)


val arite : t -> int
(** [arite mu] renvoie l'arité de [mu]. *)

val appliquer : t -> int list -> int
(** [appliquer mu l] renvoie le résultat de l'application de [mu] aux
    paramètres [l]. *)

val successeur : t
(** La fonction mu-récursive associant à tout entier naturel son successeur. *)

val constante : int -> int -> t
(** [constante a v] renvoie une fonction mu-récursive d'arité [a] renvoyant
    [v]. *)

val projection : int -> int -> t
(** [projection a i] renvoie une fonction mu-récursive d'arité [a] renvoyant
    son paramètre d'indice [i]. *)

val substitution : t -> t list -> t
(** [substitution f l] renvoie une fonction mu-récursive substituant tout
    paramètre [i] de [f] par l'application de la fonction [List.nth i l]. *)

val induction : t -> t -> t
(** [induction b h] renvoie une fonction mu-récursive d'induction avec [b]
    comme cas de base et [h] comme cas d'hérédité. *)

val minimisation : t -> t
(** [minimisation p] renvoie une fonction mu-récursive cherchant le plus petit
    entier naturel satisfiant le prédicat [p]. [p] est satisfait pour une liste
    [l] de paramètres si [p l = 0]. *)

val identite : t
(** La fonction mu-récursive d'identité. *)

val zero : t
(** La fonction mu-récursive ne prenant aucun paramètre et renvoyant zéro. *)

val print_mu : t -> unit
(** [print_mu mu] affiche la formule de [mu] dans la console.  *)

val est_identite : t list -> bool
(** [est_identite l] renvoie [true] si [l] est une liste de projections formant
    une identité et [false] sinon. *)

val substituer_parametre : (int -> t) -> int -> t -> t
(** [substituer_parametre gen i f] renvoie une fonction mu-récursive substituant
    le paramètre d'indice [i] de [f] par [gen (arite f - 1)]. *)

val modifier_seuil_inlining_induction : int -> int
(** [modifier_seuil_inlining_induction s] modifie le nombre d'itérations d'une
    fonction d'induction à partir duquel une fonction ne sera plus inlinée et
    renvoie le précédent nombre d'itérations. *)

val simplifier : t -> t
(** [simplifer mu] renvoie une fonction mu-récursive simplifiée, sémantiquement
    équivalente à [mu]. *)
