(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Inferencer
open Typing

let rec find x = function
  | [] -> 0
  | h :: t -> if x = h then 1 else 1 + find x t
;;

let inference_prog prog =
  let typs, _ =
    List.fold_left
      (fun (typs, environment) dec ->
        let rec get_last_env = function
          | [ Ok (env, _) ] -> env
          | _ :: tl -> get_last_env tl
          | _ -> environment
        in
        let tcheck dec =
          match dec with
          | DLet (_, PtWild, _) -> [ "- ", run_inference dec environment ]
          | DLet (_, PtVar name, _) -> [ name, run_inference dec environment ]
          | _ -> [ "", Error `Matching_failed ]
        in
        let environment' = get_last_env (List.map snd (tcheck dec)) in
        tcheck dec :: typs, environment')
      ([], TypeEnv.empty)
      prog
  in
  List.concat (List.rev typs)
;;

let inference _ code =
  match Parser.parse Parser.prog code with
  | Ok prog ->
    let id_x_typs = inference_prog prog in
    let only_typs = List.map snd id_x_typs in
    let error_check =
      List.find_opt
        (function
          | Error _ -> true
          | _ -> false)
        only_typs
    in
    (match error_check with
     | None ->
       List.iter
         (fun (a, b) ->
           match b with
           | Ok (_, typ) ->
             Format.printf "val %s : " a;
             print_typ typ
           | Error e -> print_type_error e)
         id_x_typs
     | Some (Error e) ->
       let index = find (Error e) only_typs in
       Format.printf "Error in №%d declaration:\n" index;
       print_type_error e
     | _ -> print_type_error `Unreachable)
  | _ -> Format.printf "Parse error\n"
;;
