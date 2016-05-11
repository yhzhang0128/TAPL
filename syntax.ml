open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty =
    TyArrow of ty * ty
  | TyBool
  | TyNat

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

(* ---------------------------------------------------------------------- *)
(* Context Management using List *)

type binding = NameBind | VarBind of ty
type context = (string * binding) list

let initialContext = []

let index2name fi ctx x = 
  if List.length ctx > x then
    let (ret,_) = List.nth ctx x in ret
  else
    error fi "index2name Fail"

let rec name2index fi ctx x = 
match ctx with
   [] -> error fi ("Identifier " ^ x ^ " is unbound")
| (y,_)::rest ->
   if y=x then 0
          else 1 + (name2index fi rest x)

let ctxlength ctx = List.length ctx

let rec pickfreshname ctx x =
    if List.exists (fun (xi,_) -> x=xi) ctx then
      pickfreshname ctx (x^"'")
    else
      ((x, NameBind)::ctx), x

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bind
  with Failure _ ->
    let msg = 
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))

let getTypeFromContext fi ctx i =
  match getbinding fi ctx i with
    VarBind(tyT) -> tyT
  | _ -> error fi
    ("getTypeFromContext: Wrong kind of binding for variable" ^ (index2name fi ctx i))
(* ---------------------------------------------------------------------- *)

(* Extracting file info *)

let tmInfo t = match t with
    TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi 
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi,_,_) -> fi


(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let rec printtm_Term outer ctx t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  | TmApp(_, t1, t2) ->
      pr "("; printtm_ATerm false ctx t1; pr " "; printtm_ATerm false ctx t2; pr ")";
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       (* Remark: print number in a friendly way  *)
       | TmSucc(_,s) -> f (n+1) s
       (* Remark: note that here uses t1 so it is correct *)
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | TmAbs(_, x, _, t1) ->
      (* push x into context for printing t1  *)
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ". "; printtm_ATerm false ctx' t1; pr ")"
  | TmVar(fi, x, n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]";
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

and printtm ctx t = match t with
  | _ -> printtm_Term true ctx t 



(* Printing Types *)
let rec printty_Type outer tyT = match tyT with
  tyT -> printty_ArrowType outer tyT

and printty_ArrowType outer tyT = match tyT with
  TyArrow(tyT_left, tyT_right) ->
    obox0();
    printty_AType false tyT_left;
    if outer then pr " ";
    pr "->";
    if outer then pr " ";
    printty_ArrowType outer tyT_right;
    cbox()
  | tyT -> printty_AType outer tyT

and printty_AType outer tyT = match tyT with
    TyBool -> pr "Bool"
  | TyNat -> pr "Nat"
  | tyT -> pr "("; printty_Type outer tyT; pr ")"

let printty tyT = printty_Type true tyT

(* ---------------------------------------------------------------------- *)
(* Substitution for evaluation *)

let termShift d t =
  let rec walk c t = match t with
     (* x is not in context, need to be shifted *)
    TmVar(fi,x,n) -> if x>=c then TmVar(fi, x+d, n+d)
                             (* x is in context *)
                             else TmVar(fi, x, n+d)
  | TmAbs(fi,x,tyT,t1) -> TmAbs(fi, x, tyT, walk(c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmIf(fi,t1,t2,t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  (* Regard succ as an abstraction *)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | _ -> t
  in walk 0 t

(* [j -> s] t *)
let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi,x,n) -> if x=j+c then termShift c s else TmVar(fi,x,n)
  | TmAbs(fi,x,tyT,t1) -> TmAbs(fi, x, tyT, walk (c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)  
  | TmIf(fi,t1,t2,t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  (* Regard succ as an abstraction *)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | _ -> t
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)
