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

type Successeur;;
type Constante;;
type Mu = Successeur | Constante of int | Composition of Mu * Mu;;


(* ------------------------- variables ------------------------- *)


exception End;;


(* ------------------------- terminal display ------------------------- *)





(* ------------------------- functions ------------------------- *)
