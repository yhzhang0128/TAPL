(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let rec process_command ctx cmd = match cmd with
  | Eval(fi,t) -> 
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      printtm ctx t'; 
      pr " : ";
      printty tyT;
      force_newline();
      initialContext
  
let process_file f ctx =
  alreadyImported := f :: !alreadyImported;
  let (cmds,_) = parseFile f ctx in
  let g ctx c =  
    open_hvbox 0;
    let results = process_command ctx c in
    print_flush();
    results
  in
    List.fold_left g ctx cmds

(*  List.iter g initialContext (match cmds with
  | [] -> failwith "TODO1"
  | _ -> cmds)*)
(* Remark: do not understand this *)
(*  | _ :: _ -> failwith "TODO2") *)
                  
let main () = 
(* (* manual test1 *)
  let term = TmAbs(dummyinfo, "x", TmVar(dummyinfo, 0, 1)) in
  let cmd = Eval(dummyinfo, term) in
    process_command cmd
*)
(* (* manual test2 *)
  let left = TmAbs(dummyinfo, "x", TmVar(dummyinfo, 0, 1)) in
  let right = TmAbs(dummyinfo, "x", TmApp(dummyinfo, TmVar(dummyinfo, 0, 1), TmVar(dummyinfo, 0, 1))) in
  let term = TmApp(dummyinfo, left, right) in
  let cmd = Eval(dummyinfo, term) in
    process_command cmd
*)
(* (* manual test3, try to change the TmVar(0, 2) to TmVar(1, 2) *)
  let term = TmAbs(dummyinfo, "x", TmAbs(dummyinfo, "x", TmVar(dummyinfo, 1, 2))) in
  let cmd = Eval(dummyinfo, term) in
    process_command cmd
*)

  let inFile = parseArgs() in
  let _ = process_file inFile initialContext in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
