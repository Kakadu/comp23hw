open MimiML_lib.Parser
open MimiML_lib.Inferencer

let infer s =
  match parse s with
  | Ok ast ->
    (match w ast with
     | Ok s -> MimiML_lib.Pp.pp_ty Format.std_formatter s
     | Error err -> pp_error Format.std_formatter err)
  | Error e -> Format.fprintf Format.std_formatter "Parsing error (%S)" e
;;

let () = infer "if 4 then 1 else 2"
