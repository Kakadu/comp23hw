open MimiML_lib.Parser
open MimiML_lib.Inferencer
open MimiML_lib.Anf
open MimiML_lib.Pp

let anf s =
  let fmt = Format.std_formatter in
  match parse s with
  | Ok ast -> pp_anf_prog fmt (anf ast)
  | Error e -> Format.fprintf fmt "Parsing error (%S)" e
;;

let () = anf (Stdio.In_channel.input_all Stdlib.stdin)
