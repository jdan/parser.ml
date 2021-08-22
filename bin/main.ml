open Lib

let () =
  match
    "(3 + 4) * 2 / 4"
    |> Util.explode
    |> Parser.apply Arith_parser.expr
  with
  | [(res, _)] -> res |> string_of_int |> print_endline
  | _ -> print_endline "Parsing error"
