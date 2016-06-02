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
  | TmAbs(fi,_,_,_) -> pr "Term Abs\n"
  | TmApp(fi,_,_) -> pr "Term App\n"
  | TmRecord(fi,_) -> pr "Term Record\n"
  | TmProj(fi,_,_) -> pr "Term Project\n"

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmAbs(_,_,_,_) -> true
  | t when isnumericval t  -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
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
  | TmApp(fi, TmAbs(_,x,_,t12), v2) when (isval ctx v2) -> 
      let rett = termSubstTop v2 t12 in
        rett
    (* val term *)
  | TmApp(fi, v1, t2) when (isval ctx v1) ->
    let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
    (* term term *)
  | TmApp(fi, t1, t2) ->
    let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)

  (* Record evaluation *)
  | TmRecord(fi, fields) ->
      let rec evalafield l = match l with
        [] -> raise NoRuleApplies
      | (l, vi)::rest when isval ctx vi ->
        let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest ->
          let ti' = eval1 ctx ti in
            (l, ti')::rest
      in let fields' = evalafield fields in
        TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
        TmProj(fi, t1', l)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t



let rec subtype tyS tyT =
  (=) tyS tyT ||
  match (tyS, tyT) with
   (* for every field in T, the corresponding field in S is corresponding subtype *)
      (TyRecord(fS), TyRecord(fT)) ->
        List.for_all
          (fun (li, tyTi) ->
             try let tySi = List.assoc li fS in
               subtype tySi tyTi
             with Not_found -> false)
         fT
    | (_, TyTop) -> true
    | (TyArrow(tyS1, tyS2), TyArrow(tyT1, tyT2)) ->
      (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
    | (_,_) -> false


let rec typeof ctx t =
  match t with
  (* Lambda Expression *)
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT_left,subt) ->
      let ctx' = addbinding ctx x (VarBind(tyT_left)) in
      let tyT_right = typeof ctx' subt in
      TyArrow(tyT_left, tyT_right)
  | TmApp(fi,t1,t2) ->
     let tyT_func = typeof ctx t1 in
     let tyT_arg = typeof ctx t2 in
     (match tyT_func with
       TyArrow(tyT_left, tyT_right) ->
     (* modified here for subtyping *)
         if subtype tyT_arg tyT_left then tyT_right
         else error fi "parameter type mismatch in TmApp"
     | _ -> error fi "arrow type expected in TmApp")

  (* Record *)
  | TmRecord(fi, fields) ->
    let fieldtys = 
      List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
    TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
    (match (typeof ctx t1) with
         TyRecord(fieldtys) ->
           (try List.assoc l fieldtys
            with Not_found -> error fi ("label" ^ l ^ " not found" )
           )
      | _ -> error fi "Expected record type in projection")

  (* Build-in functions *)
  | TmIf(fi,t1,t2,t3) ->
    if (=) (typeof ctx t1) TyBool then
      let tyT_branch0 = typeof ctx t2 in
      let tyT_branch1 = typeof ctx t3 in
        if (=) tyT_branch0 tyT_branch1 then tyT_branch0
        else error fi "type of two branches in conditional mismatch"
    else error fi "guard of conditional not a boolean"
  | TmSucc(fi,t1) ->
    let tyT = typeof ctx t1 in
      if (=) tyT TyNat then
        TyNat
      else error fi "succ get non numeral as argument"
  | TmPred(fi,t1) ->
    let tyT = typeof ctx t1 in
      if (=) tyT TyNat then
        TyNat
      else error fi "pred get non numeral as argument"
  | TmIsZero(fi,t1) -> 
    let tyT = typeof ctx t1 in
      if (=) tyT TyNat then
        TyBool
      else error fi "iszero get non numeral as argument"

  (* Values *)
  | TmTrue(fi) -> TyBool
  | TmFalse(fi) -> TyBool
  | TmZero(fi) -> TyNat
