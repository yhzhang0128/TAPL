open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let prTermType t = match t with
    TmTrue(fi) -> pr "Term True\n"
  | TmFalse(fi) -> pr "Term False\n"
  | TmIf(fi,_,_,_) -> pr "Term If\n"
  | TmZero(fi) -> pr "Term Zero\n"
  | TmSucc(fi,_) -> pr "Term Succ\n"
  | TmPred(fi,_) -> pr "Term Pred\n"
  | TmIsZero(fi,_) -> pr "Term IsZero\n"
  | TmVar(fi,_,_) -> pr "Term Var\n"
  | TmAbs(fi,_,_) -> pr "Term Abs\n"
  | TmApp(fi,_,_) -> pr "Term App\n"

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmAbs(_,_,_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  (* Lambda evaluation, call-by-value *)
    (* abs val *)
  | TmApp(fi, TmAbs(_,x,t12), v2) when (isval ctx v2) -> 
      termSubstTop v2 t12
    (* val term *)
  | TmApp(fi, v1, t2) when (isval ctx v1) ->
    let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
    (* term term *)
  | TmApp(fi, t1, t2) ->
    let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)

  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t
