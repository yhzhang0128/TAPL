open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

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

(* ---------------------------------------------------------------------- *)
(* Context Management using List *)

type binding = NameBind
type context = (string * binding) list

let initialContext = []

let index2name fi ctx x = 
  if List.length ctx > x then
    let (ret,_) = List.nth ctx x in ret
  else
    error fi "index2name Fail"

let ctxlength ctx = List.length ctx

let rec pickfreshname ctx x =
    if List.exists (fun (xi,_) -> x=xi) ctx then
      pickfreshname ctx (x^"'")
    else
      ((x, NameBind)::ctx), x


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
  | TmAbs(fi,_,_) -> fi
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

let rec printtm_Term outer t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false t1;
       print_space();
       pr "then ";
       printtm_Term false t2;
       print_space();
       pr "else ";
       printtm_Term false t3;
       cbox()
  | t -> printtm_AppTerm outer t

and printtm_AppTerm outer t = match t with
    TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false t1
  | t -> printtm_ATerm outer t

and printtm_ATerm outer t = match t with
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
       | _ -> (pr "(succ "; printtm_ATerm false t1; pr ")")
     in f 1 t1
  | t -> pr "("; printtm_Term outer t; pr ")"

let rec printtm_Lambda ctx t = match t with
    TmAbs(_, x, t1) ->
      (* push x into context for printing t1  *)
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ". "; printtm_Lambda ctx' t1; pr ")"
  | TmApp(_, t1, t2) ->
      pr "("; printtm_Lambda ctx t1; pr " "; printtm_Lambda ctx t2; pr ")";
  | TmVar(fi, x, n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]"
  | _ -> pr "printtm_Lambda: meet non-lambda term"

let termShift d t =
  let rec walk c t = match t with
     (* x is not in context, need to be shifted *)
    TmVar(fi,x,n) -> if x>=c then TmVar(fi, x+d, n+d)
                             (* x is in context *)
                             else TmVar(fi, x, n+d)
  | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk(c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)
  in walk 0 t

(* [j -> s] t *)
let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi,x,n) -> if x=j+c then termShift c s else TmVar(fi,x,n)
  | TmAbs(fi,x,t1) -> TmAbs(fi, x, walk (c+1) t1)
  | TmApp(fi,t1,t2) -> TmApp(fi, walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let printtm ctx t = match t with
    TmVar(_,_,_) -> printtm_Lambda ctx t
  | TmApp(_,_,_) -> printtm_Lambda ctx t
  | TmAbs(_,_,_) -> printtm_Lambda ctx t
  | _ -> printtm_Term true t 


