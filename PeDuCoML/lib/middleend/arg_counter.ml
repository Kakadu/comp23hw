(** Copyright 2023-2024, Danila Pechenev, Ilya Dudnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf

let gather_args_numbers (program : global_scope_function list) =
  let count_args env func =
    let id, arg_list, body = func in
    let zero_map =
      Base.List.foldi arg_list ~init:Base.Map.Poly.empty ~f:(fun ind acc arg ->
        Base.Map.Poly.set acc ~key:ind ~data:(arg, 0))
    in
    let env = Base.Map.Poly.set env ~key:id ~data:zero_map in
    let process_immexpr = function
      | ImmId id -> Some id
      | _ -> None
    in
    let rec process_cexpr result_id env partials = function
      | CApplication (func, arg) ->
        let func_id = process_immexpr func in
        let arg_id = process_immexpr arg in
        (match func_id with
         | Some func_id ->
           (match Base.Map.Poly.find partials func_id with
            | Some (func_id, current_arg_number) ->
              ( (match arg_id, func_id with
                 | Some (GlobalScopeId func_arg_name), GlobalScopeId func_name ->
                   (match Base.Map.Poly.find env func_name with
                    | Some arg_numbers_map ->
                      if Base.Map.Poly.length arg_numbers_map > current_arg_number
                      then (
                        let arg_id, _ =
                          Base.Map.Poly.find_exn arg_numbers_map current_arg_number
                        in
                        let args_number =
                          Base.Map.Poly.length (Base.Map.Poly.find_exn env func_arg_name)
                        in
                        Base.Map.Poly.set
                          env
                          ~key:func_name
                          ~data:
                            (Base.Map.Poly.set
                               arg_numbers_map
                               ~key:current_arg_number
                               ~data:(arg_id, args_number)))
                      else env
                    | None -> env)
                 | _ -> env)
              , Base.Map.Poly.set
                  partials
                  ~key:result_id
                  ~data:(func_id, current_arg_number + 1) )
            | None ->
              ( (match arg_id, func_id with
                 | Some (GlobalScopeId func_arg_name), GlobalScopeId func_name ->
                   (match Base.Map.Poly.find env func_name with
                    | Some arg_numbers_map ->
                      let arg_id, _ = Base.Map.Poly.find_exn arg_numbers_map 0 in
                      let args_number =
                        Base.Map.Poly.length (Base.Map.Poly.find_exn env func_arg_name)
                      in
                      Base.Map.Poly.set
                        env
                        ~key:func_name
                        ~data:
                          (Base.Map.Poly.set
                             arg_numbers_map
                             ~key:0
                             ~data:(arg_id, args_number))
                    | None -> env)
                 | _ -> env)
              , Base.Map.Poly.set partials ~key:result_id ~data:(func_id, 1) ))
         | None -> env, partials)
      | CIf (_, true_branch, false_branch) ->
        let env, partials = process_aexpr env partials true_branch in
        process_aexpr env partials false_branch
      | _ -> env, partials
    and process_aexpr env partials = function
      | ACExpr cexpr -> process_cexpr (AnfId 0) env partials cexpr
      | ALet (id, cexpr, aexpr) ->
        let env, partials = process_cexpr id env partials cexpr in
        process_aexpr env partials aexpr
    in
    process_aexpr env Base.Map.Poly.empty body
  in
  let intermediate_map =
    Base.List.fold program ~init:Base.Map.Poly.empty ~f:(fun acc func ->
      count_args acc func |> fst)
  in
  let rec process_one_map curr_ind final_map current_map =
    if curr_ind >= 0
    then (
      let arg_id, args_number = Base.Map.Poly.find_exn current_map curr_ind in
      process_one_map
        (curr_ind - 1)
        (Base.Map.Poly.set final_map ~key:arg_id ~data:args_number)
        current_map)
    else final_map
  in
  Base.Map.Poly.map intermediate_map ~f:(fun map ->
    process_one_map (Base.Map.Poly.length map - 1) Base.Map.Poly.empty map)
;;
