(** Copyright 2023, Lev Golofastov & Ksenia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module ClosureMap = Map.Make (String)

type closuremap = string list ClosureMap.t

let rec find_free_vars known (expr : expr) =
  let ffv = find_free_vars known in
  let bool_of_rf = function
    | RecF -> true
    | _ -> false
  in
  let pat_of_str str = str in
  match expr with
  | EConst _ -> []
  | EVar var -> [ pat_of_str var ]
  | EApp (e1, e2) -> ffv e1 @ ffv e2
  | EIfElse (e1, e2, e3) -> ffv e1 @ ffv e2 @ ffv e3
  | ELam (PatVar pat, body) ->
    List.filter (fun v -> v <> pat && List.mem v known |> not) (ffv body)
  | ELet ((rec_flag, PatVar pat, value), body) ->
    let r = bool_of_rf rec_flag in
    let known = if r then pat :: known else known in
    ffv value @ List.filter (fun v -> v <> pat && r && List.mem v known |> not) (ffv body)
;;

(* intermideate name generator *)
module NameGen = struct
  let cnt = ref 0

  let gen_name prefix =
    cnt := !cnt + 1;
    Format.sprintf "%s%d" prefix !cnt
  ;;

  let gen_name_lambda () = gen_name "lambda@"
  let gen_name_im () = gen_name "i@"

  let gen_number () =
    cnt := !cnt + 1;
    string_of_int (!cnt + 1)
  ;;
end

let fresh_name var = var ^ "_" ^ NameGen.gen_number ()

type clres = expr -> closuremap * expr * (string * expr) list

(* Name (a) => List (f_a, f_g_a, f_g_a_1 etc)*)
type rename_map = string list ClosureMap.t

let rec rename_ast ast traceback used_vars =
  let traceback_name = List.fold_left (Format.sprintf "%s%s@") "" (List.rev traceback) in
  let rename_new_var v used_vars =
    let rec rename_used new_name =
      let tb_name = traceback_name ^ new_name in
      match ClosureMap.find_opt tb_name used_vars with
      | Some x -> rename_used (List.hd x ^ "@")
      | None -> tb_name
    in
    rename_used v
  in
  let rename_var v =
    match ClosureMap.find_opt v used_vars with
    | Some [] -> v
    | Some names -> List.hd names
    | None -> v
  in
  let uv_with old_name new_name =
    match ClosureMap.find_opt old_name used_vars with
    | None -> ClosureMap.add old_name [ new_name ] used_vars
    | Some other_names -> ClosureMap.add old_name (new_name :: other_names) used_vars
  in
  match ast with
  | EConst c -> EConst c
  | EVar v -> EVar (rename_var v)
  | EApp (fn, args) ->
    EApp (rename_ast fn traceback used_vars, rename_ast args traceback used_vars)
  | EIfElse (i, t, e) ->
    EIfElse
      ( rename_ast i traceback used_vars
      , rename_ast t traceback used_vars
      , rename_ast e traceback used_vars )
  | ELam (PatVar p, e) ->
    let new_name = rename_new_var p used_vars in
    ELam (PatVar new_name, rename_ast e traceback (uv_with p new_name))
  | ELet ((rf, PatVar v, inner_e), outer_e) ->
    let new_name = rename_new_var v used_vars in
    let inner_uv =
      match rf with
      | RecF -> uv_with v new_name
      | NRecF -> used_vars
    in
    let inner_e = rename_ast inner_e (v :: traceback) inner_uv in
    let outer_e = rename_ast outer_e traceback (uv_with v new_name) in
    ELet ((rf, PatVar new_name, inner_e), outer_e)
;;

(* closure: expr -> fun list * expr *)
let rec closure (known : string list) (cm : closuremap) : clres =
  let klosure = closure in
  let closure = closure known in
  let name_and_lift known arg body name =
    let free_vars = ELam (PatVar arg, body) |> find_free_vars known in
    let cm = ClosureMap.add name free_vars cm in
    let cm, body, lftd_b = closure cm body in
    (* add args *)
    let lifted_body =
      List.fold_left (fun x y -> ELam (PatVar y, x)) (ELam (PatVar arg, body)) free_vars
    in
    let _, clsr, _ = EVar name |> closure cm in
    cm, clsr, lftd_b @ [ name, lifted_body ]
  in
  function
  | EConst c -> cm, EConst c, []
  | EVar var ->
    let apply fn arg = EApp (fn, EVar arg) in
    (match ClosureMap.find_opt var cm with
     (* var is present in closures we found, replace var with var x y z (x y z are in free vars) *)
     | Some args -> cm, List.fold_left apply (EVar var) args, []
     (* var is either a simple var or a closure with no unbound vars, leaving just var *)
     | None -> cm, EVar var, [])
  | EApp (ex1, ex2) ->
    let cm, res1, lftd1 = closure cm ex1 in
    let cm, res2, lftd2 = closure cm ex2 in
    cm, EApp (res1, res2), lftd1 @ lftd2
  | EIfElse (ex1, ex2, ex3) ->
    let cm, res1, lftd1 = closure cm ex1 in
    let cm, res2, lftd2 = closure cm ex2 in
    let cm, res3, lftd3 = closure cm ex3 in
    cm, EIfElse (res1, res2, res3), lftd1 @ lftd2 @ lftd3
  | ELet ((r, PatVar name, ELam (PatVar arg, expr)), body) ->
    let known =
      match r with
      | RecF -> name :: known
      | _ -> known
    in
    let cm, _, l = name_and_lift known arg expr name in
    let cm, body, lifted = closure cm body in
    cm, body, l @ lifted
  | ELam (PatVar pat, expr) -> name_and_lift known pat expr (NameGen.gen_name_lambda ())
  | ELet ((r, PatVar pat, value), body) ->
    let known =
      match r with
      | RecF -> pat :: known
      | _ -> known
    in
    let cm, value_res, lftd_v = klosure known cm value in
    let cm, body_res, lftd_b = klosure (pat :: known) cm body in
    cm, ELet ((r, PatVar pat, value_res), body_res), lftd_v @ lftd_b
;;

let closure ast =
  let ops = [ "+"; "-"; "*"; "/"; "<"; ">"; ">="; "<="; "=" ] in
  rename_ast ast [] ClosureMap.empty |> closure ops ClosureMap.empty
;;

type immexpr =
  | ImmValue of string (* Variable *)
  | ImmNum of int (* Constants *)
  | ImmString of string
  | ImmBool of bool
  | ImmUnit

type cexpr =
  | CImm of immexpr (* Constant or variable *)
  | CApp of immexpr * immexpr (* Function application *)
  | CIfElse of immexpr * ablock * ablock (* If-then-else *)

and alet = string * cexpr (* Single assignment a = 3 * 2 *)
and ablock = immexpr * alet list (* ANF Block: a = 3; b = 2 + 1; c = a + b; c *)

type aprogram = ablock (* Whole program *)
type afun = string * string list * ablock (* Functions *)

let rec anf prog : expr -> aprogram = function
  | EConst (CInt i) -> ImmNum i, prog
  | EConst (CBool b) -> ImmBool b, prog
  | EConst (CString s) -> ImmString s, prog
  | EConst CUnit -> ImmUnit, prog
  | EApp (fn, arg) ->
    let name = NameGen.gen_name_im () in
    let fn_imm, fn_prog = anf prog fn in
    let arg_imm, arg_prog = anf prog arg in
    let prog = fn_prog @ arg_prog @ [ name, CApp (fn_imm, arg_imm) ] in
    ImmValue name, prog
  | EIfElse (i, t, e) ->
    let name = NameGen.gen_name_im () in
    let i_imm, i_prog = anf prog i in
    let t_imm, t_prog = anf prog t in
    let e_imm, e_prog = anf prog e in
    let prog = i_prog @ [ name, CIfElse (i_imm, (t_imm, t_prog), (e_imm, e_prog)) ] in
    ImmValue name, prog
  | ELet ((_, PatVar name, body), in_e) ->
    let body_imm, body_prog = anf prog body in
    let body_prog = body_prog @ [ name, CImm body_imm ] in
    let in_e_imm, in_e_prog = anf prog in_e in
    let prog = body_prog @ in_e_prog in
    in_e_imm, prog
  | EVar v -> ImmValue v, prog
  | ELam _ -> failwith "closure conversion not performed"
;;

let anf_fun name : expr -> afun = function
  | ELam (PatVar a, e) ->
    let rec get_args = function
      | ELam (PatVar a, e) ->
        let args, e = get_args e in
        a :: args, e
      | other -> [], other
    in
    let all_args, expr = get_args (ELam (PatVar a, e)) in
    let res, prog = anf [] expr in
    name, all_args, (res, prog)
  | e -> name, [], anf [] e
;;

let anf : expr -> afun list * aprogram =
  fun ast ->
  let _, expr, lifted = closure ast in
  List.map (fun (x, y) -> anf_fun x y) lifted, anf [] expr
;;
