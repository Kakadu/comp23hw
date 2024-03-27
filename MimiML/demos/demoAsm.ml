open MimiML_lib.Parser
open MimiML_lib.Inferencer
open MimiML_lib.Anf
open MimiML_lib.Asm

let compile s =
  match parse s with
  | Ok ast ->
    let afuns, main = anf ast in
    compile afuns main
  | Error e -> Format.fprintf Format.std_formatter "Parsing error (%S)" e
;;

let () = compile (Stdio.In_channel.input_all Stdlib.stdin)
