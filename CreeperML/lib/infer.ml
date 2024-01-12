(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferUtils = struct
  open Type_ast.InferType
  open Type_ast.InferTypeUtils
  open Monad.Result
  open Position.Position

  (* inner module for inferencer *)

  (* naming and global lvl management*)
  let generic_lvl = 100500
  let marked_lvl = -1
  let gensym_counter = ref 0
  let reset_gensym () = gensym_counter := 0

  let gensym () =
    let n = !gensym_counter in
    let () = incr gensym_counter in
    if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
    else "t" ^ string_of_int n

  let curr_lvl = ref 0
  let reset_curr_lvl () = curr_lvl := 0

  let reset_typ_vars () =
    reset_gensym ();
    reset_curr_lvl ()

  let enter_lvl () = incr curr_lvl
  let leave_lvl () = decr curr_lvl

  (* type var gen *)
  let new_var () =
    let name = gensym () in
    tv_unbound name !curr_lvl |> ref |> t_var |> with_lvls !curr_lvl !curr_lvl

  let new_arrow l r = t_arrow l r |> with_lvls !curr_lvl !curr_lvl
  let new_tuple ts = t_tuple ts |> with_lvls !curr_lvl !curr_lvl

  (* checks t for cycles *)
  let rec cyc_free t =
    match lvl_value t with
    | TVar { contents = Link t } -> cyc_free t
    | (TArrow _ | TTuple _) when t.new_lvl = marked_lvl -> error "occurse fail"
    | TArrow (l, r) ->
        let lvl = t.new_lvl in
        t.new_lvl <- marked_lvl;
        cyc_free l *> cyc_free r
        *>
        (t.new_lvl <- lvl;
         return t)
    | TTuple ts ->
        let lvl = t.new_lvl in
        t.new_lvl <- marked_lvl;
        List.fold_left
          (fun acc t -> acc *> cyc_free t)
          (List.hd ts |> cyc_free)
          (List.tl ts)
        *>
        (t.new_lvl <- lvl;
         return t)
    | _ -> return t

  (* lvlv update *)
  let lvls_to_update = ref []
  let reset_lvls_to_update () = lvls_to_update := []

  (* updates lvl of type var *)
  let update_lvl l t =
    match lvl_value t with
    | TVar ({ contents = Unbound (n, lvl) } as tvar) ->
        if lvl >= generic_lvl then error "missed invariant"
        else if l < lvl then (
          tvar := tv_unbound n lvl;
          return t)
        else return t
    | TArrow _ | TTuple _ ->
        if t.new_lvl >= generic_lvl then error "missed invariant"
        else if t.new_lvl = marked_lvl then error "occurse fail"
        else if l < t.new_lvl && t.new_lvl = t.old_lvl then
          return
            (lvls_to_update := t :: !lvls_to_update;
             t.new_lvl <- l;
             t)
        else return t
    | _ -> return t

  (* forses lvl update to type vars in backlog *)
  let force_lvls_update () =
    let rec helper acc level t =
      let t = repr t in
      match lvl_value t with
      | TVar ({ contents = Unbound (name, l) } as tvar) when l > level ->
          tvar := tv_unbound name level;
          acc
      | (TArrow _ | TTuple _) when t.new_lvl = marked_lvl ->
          error "occurse fail"
      | TArrow _ | TTuple _ ->
          if t.new_lvl > level then t.new_lvl <- level;
          update_one acc t
      | _ -> acc
    and update_one acc t =
      match lvl_value t with
      | (TArrow _ | TTuple _) when t.old_lvl <= !curr_lvl ->
          let* acc = acc in
          return (t :: acc)
      | (TArrow _ | TTuple _) when t.old_lvl = t.new_lvl -> acc
      | TArrow (l, r) ->
          let lvl = t.new_lvl in
          t.new_lvl <- marked_lvl;
          let acc = helper acc lvl l in
          let acc = helper acc lvl r in
          t.new_lvl <- lvl;
          t.old_lvl <- lvl;
          acc
      | TTuple ts ->
          let lvl = t.new_lvl in
          t.new_lvl <- marked_lvl;
          let acc = List.fold_left (fun acc t -> helper acc lvl t) acc ts in
          t.new_lvl <- lvl;
          t.old_lvl <- lvl;
          acc
      | _ -> error "never happen update_one error"
    in
    let* ls_to_update = List.fold_left update_one (return []) !lvls_to_update in
    return (lvls_to_update := ls_to_update)

  (* unifies two type vars and returned their general type *)
  let rec unify (p1, p2) t1 t2 =
    if t1 == t2 then return t1
    else
      let t1, t2 = (repr t1, repr t2) in
      match (lvl_value t1, lvl_value t2) with
      | ( TVar ({ contents = Unbound (_, l1) } as tv1),
          TVar ({ contents = Unbound (_, l2) } as tv2) ) ->
          if tv1 == tv2 then return t1 (* think here *)
          else
            return
              (if l1 > l2 then (
                 tv1 := tv_link t2;
                 t2)
               else (
                 tv2 := tv_link t1;
                 t1))
      | TVar ({ contents = Unbound (_, l) } as tvar), _ ->
          update_lvl l t2 >>| fun t ->
          tvar := tv_link t;
          t1
      | _, TVar ({ contents = Unbound (_, l) } as tvar) ->
          update_lvl l t1 >>| fun t ->
          tvar := tv_link t;
          t2
      | TArrow (l_t1, l_t2), TArrow (r_t1, r_t2) ->
          if t1.new_lvl = marked_lvl || t2.new_lvl = marked_lvl then
            error "occurse fail"
          else
            let min_lvl = min t1.new_lvl t2.new_lvl in
            t1.new_lvl <- marked_lvl;
            t2.new_lvl <- marked_lvl;
            let* fst = unify_lev (p1, p2) min_lvl l_t1 r_t1 in
            let* snd = unify_lev (p1, p2) min_lvl l_t2 r_t2 in
            return
              (t1.new_lvl <- min_lvl;
               t2.new_lvl <- min_lvl;
               new_arrow fst snd) (* here think too about lvlvs*)
      | TTuple l_ts, TTuple r_ts ->
          if t1.new_lvl = marked_lvl || t2.new_lvl = marked_lvl then
            error "occurse fail"
          else
            let min_lvl = min t1.new_lvl t2.new_lvl in
            t1.new_lvl <- marked_lvl;
            t2.new_lvl <- marked_lvl;
            let* ts =
              List.fold_right2
                (fun l_t r_t acc ->
                  let* acc = acc in
                  unify_lev (p1, p2) min_lvl l_t r_t >>| fun t -> t :: acc)
                l_ts r_ts (return [])
            in
            return
              (t1.new_lvl <- min_lvl;
               t2.new_lvl <- min_lvl;
               new_tuple ts)
      | TGround l_t, TGround r_t when l_t = r_t -> return t1
      | _ ->
          Printf.sprintf "cant unify %s\n at %s and\n %s at %s" (show_typ t1)
            (show_loc p1) (show_typ t2) (show_loc p2)
          |> error

  and unify_lev pos l t1 t2 =
    repr t1 |> update_lvl l >>= fun t1 -> unify pos t1 t2

  (* generalise type vars *)
  let gen t =
    let rec helper t =
      let t = repr t in
      let get_lvl t =
        match lvl_value t with
        | TVar { contents = Unbound (_, l) } -> l
        | TArrow _ | TTuple _ -> t.new_lvl
        | _ -> 0
      in
      match lvl_value t with
      | TVar ({ contents = Unbound (n, l) } as tvar) when l > !curr_lvl ->
          tvar := tv_unbound n generic_lvl;
          return t
      | TArrow (l_t, r_t) when t.new_lvl > !curr_lvl ->
          let l_t, r_t = (repr l_t, repr r_t) in
          let* _ = helper l_t in
          let* _ = helper r_t in
          let l = max (get_lvl l_t) (get_lvl r_t) in
          t.new_lvl <- l;
          t.old_lvl <- l;
          return t
      | TTuple ts when t.new_lvl > !curr_lvl ->
          let l =
            List.fold_left
              (fun acc t ->
                let t = repr t in
                let _ = helper t in
                get_lvl t |> max acc)
              0 ts
          in
          t.new_lvl <- l;
          t.old_lvl <- l;
          return t
      | _ -> return t
    in
    force_lvls_update () *> helper t

  let inst =
    let rec helper sb t =
      match lvl_value t with
      | TVar { contents = Unbound (n, l) } when l = generic_lvl -> (
          try (assoc n sb, sb)
          with Not_found ->
            let t = new_var () in
            (t, (n, t) :: sb))
      | TVar { contents = Link t } -> helper sb t
      | TArrow (l, r) when t.new_lvl = generic_lvl ->
          let l, sb = helper sb l in
          let r, sb = helper sb r in
          (new_arrow l r, sb)
      | TTuple ts when t.new_lvl = generic_lvl ->
          let ts, sb =
            List.fold_right
              (fun t (ts, sb) ->
                let t, sb = helper sb t in
                (t :: ts, sb))
              ts ([], sb)
          in
          (new_tuple ts, sb)
      | _ -> (t, sb)
    in
    fun t -> helper [] t |> fst
end

module Infer = struct
  open Parser_ast.ParserAst
  open Parser_ast.ParserAstUtils
  open InferUtils
  open Type_ast.InferType
  open Type_ast.InferTypeUtils
  open Type_ast.TypeAstUtils
  open Monad.Result
  open Position.Position

  (* main inferencer module *)

  (* types for inner functions *)
  module Named = struct
    type named = Named of name | NotNamed | NamedTuple of named list

    let named n = Named n
    let not_named = NotNamed
    let named_tuple ns = NamedTuple ns
  end

  let rec lvalue lv =
    let open Named in
    match value lv with
    | LvAny -> (not_named, new_var ()) |> return
    | LvUnit ->
        (not_named, t_ground t_unit |> with_lvls !curr_lvl !curr_lvl) |> return
    | LvValue n -> (named n, new_var ()) |> return
    | LvTuple lvs ->
        List.fold_right
          (fun lv acc ->
            let* acc = acc in
            lvalue lv >>| fun n -> n :: acc)
          lvs (return [])
        >>| List.split
        >>| fun (ns, ts) -> (named_tuple ns, new_tuple ts)

  let rec bind_lv_typ env lv t =
    let open Named in
    let list_combine ns ts =
      match Base.List.zip ns ts with
      | Base.List.Or_unequal_lengths.Unequal_lengths ->
          error
            "count of names in tuple are not same with count of returned \
             expressions"
      | Base.List.Or_unequal_lengths.Ok ps -> return ps
    in
    match lv with
    | Named n -> (n, t) :: env |> return
    | NotNamed -> return env
    | NamedTuple ns -> (
        match lvl_value t with
        | TTuple ts ->
            list_combine ns ts
            >>= List.fold_left
                  (fun acc (n, t) ->
                    let* acc = acc in
                    bind_lv_typ acc n t)
                  (return env)
        | _ -> error "isnt tuple")

  module ExprRet = struct
    (* type of value returned by tof_expr *)
    type expr_ret = {
      expr_typ : Type_ast.InferType.typ;
      expr_ast : Type_ast.InferType.typ Type_ast.TypeAst.typ_expr;
    }

    let ret_typ { expr_typ = t; expr_ast = _ } = t
    let ret_ast { expr_typ = _; expr_ast = t } = t
  end

  module LetRet = struct
    (* type of value returned by tof_let *)
    type let_ret = {
      let_typ : Type_ast.InferType.typ;
      let_ast : Type_ast.InferType.typ Type_ast.TypeAst.typ_let_binding;
    }

    let ret_let { let_typ = _; let_ast = t } = t
  end

  let rec tof_expr env expr =
    let open ExprRet in
    let open LetRet in
    match value expr with
    | ELiteral l ->
        convert_const l |> with_lvls !curr_lvl !curr_lvl |> fun expr_typ ->
        value l |> t_literal |> with_typ expr_typ |> fun expr_ast ->
        return { expr_typ; expr_ast }
    | EValue n -> (
        try
          assoc n env |> inst |> fun expr_typ ->
          t_value n |> with_typ expr_typ |> fun expr_ast ->
          return { expr_typ; expr_ast }
        with Not_found -> Printf.sprintf "cant find name %s" n |> error)
    | ETuple es ->
        let* es = tof_expr env |> monadic_map es in
        List.map ret_typ es |> new_tuple |> fun expr_typ ->
        List.map ret_ast es |> t_tuple |> with_typ expr_typ |> fun expr_ast ->
        return { expr_typ; expr_ast }
    | EFun f ->
        let* n, t_arg = lvalue f.lvalue in
        let* env = bind_lv_typ env n t_arg in
        let* let_ast, env =
          f.body |> value |> lets
          |> List.fold_left
               (fun acc l ->
                 let* lets, env = acc in
                 tof_let env l >>| fun ({ let_typ = _; let_ast }, env) ->
                 (let_ast :: lets, env))
               (return ([], env))
          >>| fun (let_ast, env) -> (List.rev let_ast, env)
        in
        let* env = bind_lv_typ env n t_arg in
        let* t_body = f.body |> value |> expr_b |> tof_expr env in
        new_arrow t_arg t_body.expr_typ |> fun expr_typ ->
        value f.lvalue |> with_typ t_arg |> fun l_v ->
        typ_let_body let_ast t_body.expr_ast |> t_fun l_v |> with_typ expr_typ
        |> fun expr_ast -> return { expr_typ; expr_ast }
    | EApply (l, r) ->
        let* t_fun = tof_expr env l in
        let* t_arg = tof_expr env r in
        let t_res = new_var () in
        let* _ =
          new_arrow t_arg.expr_typ t_res |> unify (l.pos, r.pos) t_fun.expr_typ
        in
        t_apply t_fun.expr_ast t_arg.expr_ast |> with_typ t_res
        |> fun expr_ast -> return { expr_typ = t_res; expr_ast }
    | EIfElse { cond = c; t_body = t; f_body = f } ->
        let* t_c = tof_expr env c in
        let* _ =
          t_bool |> t_ground
          |> with_lvls !curr_lvl !curr_lvl
          |> unify (c.pos, c.pos) t_c.expr_typ
        in
        let* t_t = tof_expr env t in
        let* t_f = tof_expr env f in
        let* expr_typ =
          unify (t.pos, f.pos) t_t.expr_typ t_f.expr_typ (* think here *)
        in
        t_if_else t_c.expr_ast t_t.expr_ast t_f.expr_ast |> with_typ expr_typ
        |> fun expr_ast -> return { expr_typ; expr_ast }

  (* type of let expression *)
  and tof_let env { value = { rec_f; l_v; args; body }; pos = _ } =
    let open ExprRet in
    let open LetRet in
    let* rec_env =
      (if is_rec rec_f then lvalue l_v >>= fun (n, t) -> bind_lv_typ env n t
       else return env)
      |> fun env ->
      List.fold_left
        (fun env arg ->
          let* env = env in
          lvalue arg >>= fun (n, t) -> bind_lv_typ env n t)
        env args
    in
    enter_lvl ();
    let* lets, inner_env =
      List.fold_left
        (fun acc l ->
          let* lets, env = acc in
          let* l, env = tof_let env l in
          return (l :: lets, env))
        (return ([], rec_env))
        body.value.lets
    in
    let* t_e = body.value.expr |> tof_expr inner_env in
    leave_lvl ();
    let lets = List.rev lets |> List.map ret_let in
    gen t_e.expr_typ >>= cyc_free >>= fun expr_typ ->
    typ_let_body lets t_e.expr_ast
    |> typ_let_binding rec_f (value l_v |> with_typ expr_typ)
    |> fun let_ast ->
    lvalue l_v >>= fun (n, _) ->
    bind_lv_typ rec_env n expr_typ >>| fun env ->
    ({ let_typ = expr_typ; let_ast }, env)

  (* top level expr infer *)
  let top_expr_infer env expr =
    reset_typ_vars ();
    reset_lvls_to_update ();
    tof_expr env expr >>| fun e -> ExprRet.ret_ast e |> convert_expr

  (* top level let binding infer *)
  let top_let_infer env l =
    reset_typ_vars ();
    reset_lvls_to_update ();
    tof_let env l >>| fun (l, _) -> LetRet.ret_let l |> convert_let

  (* top level inferencer *)
  let top_infer env prog =
    reset_typ_vars ();
    reset_lvls_to_update ();
    let* prog, _ =
      List.fold_left
        (fun acc l ->
          let* prog, env = acc in
          let* t, env = tof_let env l in
          (t.let_ast :: prog, env) |> return)
        (return ([], env))
        prog
    in
    List.rev prog |> List.map convert_let |> return
end
