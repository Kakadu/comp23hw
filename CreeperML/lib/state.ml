(** Copyright 2023-2024, Arthur Alekseev and Starcev Matvey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type state = { next : int }
type 'a t = state -> 'a * state

let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
 fun state ->
  let a, transient_state = t state in
  let b, final_state = f a transient_state in
  (b, final_state)

let ( >>= ) = bind
let ( let* ) = ( >>= )
let return (a : 'a) (state : state) = (a, state)

let new_var prefix (state : state) =
  let var = Format.sprintf "%s_%d" prefix state.next in
  let state = { next = state.next + 1 } in
  (var, state)

let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
 fun m f state ->
  let v, s = m state in
  (f v, s)

let ( |<< ) x y = y >>| x

let monadic_map l f =
  List.fold_right
    (fun e acc ->
      let* acc = acc in
      f e >>| fun e -> e :: acc)
    l (return [])

let monadic_mapi l f =
  List.fold_left
    (fun (acc, index) e ->
      ( (let* acc = acc in
         f index e >>| fun e -> e :: acc),
        index + 1 ))
    (return [], 0)
    l
  |> fst

let monadic_fold f initial lst =
  List.fold_left (fun acc x -> acc >>= fun acc -> f acc x) (return initial) lst

let run : 'a t -> 'a = fun m -> m { next = 0 } |> fst
