infixr 9 of
infixl 8 * / rem
infixl 7 + - ^
infixr 6 : ++
infixl 5 == <> < > <= >=
infixr 4 :=
infixr 3 &&
infixr 2 ||
infixl 0 |>
infixr 0 $

datatype bool = False | True
datatype * list = NIL | : (*, * list)
datatype * option = NONE | SOME *
datatype (*, err) result = OK * | ERROR err
datatype * ref = REF *


#
# BOOL
#

let not x = if x then False else True


#
# CHAR
#

let islower c = let n = ord c in n >= ord 'a' && n <= ord 'z'
let isupper c = let n = ord c in n >= ord 'A' && n <= ord 'Z'
let isalpha c = islower c || isupper c
let isdigit c = let n = ord c in n >= ord '0' && n <= ord '9'
let isalnum c = isalpha c || isdigit c


#
# FUNCTION
#

let identity x = x
let const x _ = x
let flip f x y = f y x
let uncurry f (x, y) = f x y
let curry f x y = f(x, y)


#
# GENERAL
#

let pr x = print x; print "\n"; x
let ! (REF value) = value
let ref value = REF value
let of(f, g) x = f (g x)
let |>(x, f) = f x
let $(f, x) = f x


#
# INTEGER
#

let negate x = 0 - x
let abs x = if x < 0 then negate x else x
let min x y = if x < y then x else y
let max x y = if x > y then x else y


#
# OPTION
#

let is_some opt = case opt | SOME _ -> True | _ -> False
let is_none opt = case opt | NONE -> True | _ -> False

# Working with options
# FUNCTION        SOME    RETURN    NONE  RESULT
# fold_option     value   value     func  any
# bind_option     value   option    -     option
# map_option      value   value     -     option

let fold_option some none opt =
  case opt
  | SOME x -> some x
  | NONE   -> none ()
let get_value opt default = fold_option identity (const default) opt
let bind_option some opt = fold_option some (const NONE) opt
let map_option some opt = fold_option (SOME of some) (const NONE) opt


#
# RESULT
#

let get_ok (OK value) = value
let get_error (ERROR error) = error

let fold_result ok error result =
  case result
  | OK value    -> ok
  | ERROR value -> error value

let bind_result ok result = fold_result ok identity result
let map_result ok result = fold_result (OK of ok) identity result





# LIST
let foldl f leftmost list =
  let rec loop xs out =
    case xs
    | x : xs' -> loop xs' (f out x)
    | []      -> out
  in
  loop list leftmost

let foldl_self f list =
  let (leftmost : list') = list in
  foldl f leftmost list'

let reverse list = foldl (fn x y -> y : x) [] list

let foldr f rightmost list =
  foldl (flip f) rightmost (reverse list)

let foldr_self f list =
  let (rightmost : list') = reverse list in
  foldl (fn x y -> f y x) rightmost list'

let ++(lhs, rhs) = foldr (curry (:)) rhs lhs
let flatten lists = foldr (curry (++)) [] lists

let null list =
  case list
  | [] -> True
  | _  -> False

let length list =
  let rec loop list n =
    case list
    | _ : list' -> loop list' (n + 1)
    | []        -> n
  in
  loop list 0

let hd(x : _) = x
let tl(_ : x) = x
let last list = foldr_self (fn _ x -> x) list

let rec nth list i =
  if i == 0 then
    let (x : _) = list in x
  else
    case list
    | _ : list' -> nth list' (i - 1)
    | []        -> raise "SUBSCRIPT"


let map f list = foldr (fn x xs -> f x : xs) [] list

let find p list =
  let rec loop list =
    case list
    | x : list' -> if p x then SOME x else loop list'
    | []        -> NONE
  in
  loop list

let any p list = is_some (find p list)
let all p list = not (any (not of p) list)
let none p list = not (any p list)

let filter p list =
  foldr (fn x xs -> if p x then x : xs else xs) [] list

let partition p list =
  let rec loop list lhs rhs =
    case list
    | x : list' -> if p x then loop list' (x : lhs) rhs else
                               loop list' lhs (x : rhs)
    | []        -> (lhs, rhs)
  in
  loop list [] []

let zip lhs rhs =
  let rec loop lhs rhs out =
    case (lhs, rhs)
    | ([], _) -> out
    | (_, []) -> out
    | (lhs : lhs', rhs : rhs') -> loop lhs' rhs' ((lhs, rhs) : out)
  in
  reverse (loop lhs rhs [])

let unzip list =
  foldr (fn (l, r) (ls, rs) -> (l : ls, r : rs)) ([], []) list

# STRING
let explode string =
  let rec loop i out =
    let c = sub(string, i) in
    if c == '\0' then reverse out else loop (i + 1) (c : out)
  in
  loop 0 []

let ^(lhs, rhs) = concat [lhs, rhs]

let split delim string =
  let rec loop i j out =
    let c = sub(string, j) in
    if c == '\0'  then let field = substring(string, i, j - i) in
                       (field : out)
                  else
    if c == delim then let field = substring(string, i, j - i) in
                       loop (j + 1) (j + 1) (field : out)
                  else
    loop i (j + 1) out
  in
  reverse (loop 0 0 [])


# INTEGER (cont.)

let maximum list = foldl_self max list
let minimum list = foldl_self min list

# int-to-ASCII
let rec itoa n =
  if n == 0 then "0" else
  if n < 0 then "-" ^ itoa (negate n) else
  let rec loop n out =
    if n == 0 then
      implode out
    else
      let c = chr (n rem 10 + ord '0') in
      loop (n / 10) (c : out)
  in
  loop n []

# ASCII-to-int
let atoi text =
  let rec loop i out =
    let c = sub(text, i) in
    if isdigit c then
      loop (i + 1) (out * 10 + (ord c - ord '0'))
    else
      out
  in
  if sub(text, 0) == '-' then negate (loop 1 0) else
  if sub(text, 0) == '+' then loop 1 0 else
  loop 0 0



