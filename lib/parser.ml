type 'a t = Parser of (char list -> ('a * char list) list)

(* The first parser we define is item, which successfully consumes the first
   character if the argument string is non-empty, and fails otherwise
*)
let item = Parser (function
    | [] -> []
    | c::cs -> [(c, cs)]
  )

let parse (Parser p) = p

(* Parser is a monad *)
let return a : 'a t = Parser (fun str -> [(a, str)])
let (>>=) p f : 'b t = Parser (
    fun str ->
      parse p str
      |> List.map (fun (a, cs) -> parse (f a) cs)
      |> List.concat
  )
let map f p = Parser (
    fun str ->
      parse p str
      |> List.map (fun (a, cs) -> (f a, cs))
  )

(* Define a monadic let *)
let (let*) = (>>=)
let (let+) p f = map f p

let zero = Parser (fun _ -> [])
let (++) p q =
  Parser (fun cs -> List.concat [parse p cs; parse q cs])
(* For this reason, we define a (deterministic) choice operator (+++) that has
   the same behaviour as (++), except that at most one result is returned
*)
let (+++) p q =
  Parser (fun cs -> match parse (p ++ q) cs with
      | [] -> []
      | x::_ -> [x]
    )

(* The item parser consumes single characters unconditionally. To allow
   conditional parsing, we define a combinator sat that takes a predicate,
   and yields a parser that consumes a single character if it satisfies the
   predicate, and fails otherwise
*)
let sat p =
  let* c = item in
  if p c
  then return c
  else zero

(* Example: a parser for specific characters can be defined as follows *)
let char c = sat ((==) c)

(* Recursive combinators *)
(* Parse a specific string *)
let rec string = function
  | [] -> return []
  | c :: cs ->
    let* _ = char c in
    let+ _ = string cs in
    c :: cs

(* Parse repeated applications of a parser p; the many combinator permits zero
   or more applications of p, while many1 permits one or more
*)
let rec many p = many1 p +++ return []
and many1 p =
  let* fst = p in
  let+ rest = many p in
  fst :: rest

(* Parse repeated applications of a parser p, separated by applications of a
   parser sep whose result values are thrown away
*)
let rec sepby p sep =
  sepby1 p sep +++ return []
and sepby1 p sep =
  let* fst = p in
  let+ rest = many (let* _ = sep in p) in
  fst :: rest

(* Parse repeated applications of a parser p, separated by applications of a
   parser op whose result value is an operator that is assumed to associate
   to the left, and which is used to combine the results from the p parsers
*)
let rec chainl p op a =
  chainl1 p op +++ return a
and chainl1 p op =
  let rec rest a =
    ( let* f = op in
      let* b = p in
      rest (f a b)
    ) +++ return a
  in
  let* a = p in
  rest a

(* Lexical combinators *)
(* Parse a string of spaces, tabs, and newlines *)
let is_space = function
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | _ -> false

let space = many (sat is_space)
let non_space = many1 (sat (fun c -> not (is_space c)))

(* Parse a token using a parser p, throwing away any trailing space *)
let token p =
  let* a = p in
  let+ _ = space in
  a

(* Parse a symbolic token *)
let symb cs = token (string cs)

(* Apply a parser p, throwing away any leading space *)
let apply p =
  parse (let* _ = space in p)
