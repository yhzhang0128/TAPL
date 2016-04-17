(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
(* New term definition for lambda, in de Bruijn notation  *)
  | TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term


type command =
  | Eval of info * term


type binding = NameBind
type context = (string * binding) list

val initialContext: context


(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> term -> unit
val printtm_Lambda: context -> term -> unit

(* Lambda syntax *)
val termShift: int -> term -> term
val termSubst: int -> term -> term -> term
val termSubstTop: term -> term -> term

(* Misc *)
val tmInfo: term -> info

