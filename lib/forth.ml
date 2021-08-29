type operator = Add | Sub | Mul
type token =
  | Operator of operator
  | Identifier of string
  | Number of int
type statement = token list

open Parser
let digit =
  let is_digit c = c >= '0' && c <= '9' in
  sat is_digit
let number =
  let+ cs = many1 digit
  in Number (cs |> Util.implode |> int_of_string)

let identifer =
  let+ cs = non_space
  in Identifier (cs |> Util.implode)

let operator =
  let+ oper = (
    (let+ _ = char '+' in Add)
    +++ (let+ _ = char '-' in Sub)
    +++ (let+ _ = char '*' in Mul)
  ) in Operator oper

let token = operator +++ number +++ identifer

let statement = sepby token space

exception ParseException
let parse str =
  match str |> Util.explode |> apply statement with
  | [] -> raise ParseException
  | (ts, _) :: _ -> ts
