(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

type ty =
    TyArrow of ty * ty
  | TyBool
  | TyNat

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
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term


type command =
  | Eval of info * term


type binding = NameBind | VarBind of ty
type context = (string * binding) list

val initialContext: context
val index2name: info -> context -> int -> string
val name2index: info -> context -> string -> int
val ctxlength: context -> int
val addname: context -> string -> context

val addbinding: context -> string -> binding -> context
val getbinding: info -> context -> int -> binding
val getTypeFromContext: info -> context -> int -> ty

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit

val printty: ty -> unit

(* Lambda syntax *)
val termShift: int -> term -> term
val termSubst: int -> term -> term -> term
val termSubstTop: term -> term -> term

(* Misc *)
val tmInfo: term -> info

