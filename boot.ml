infixr 10 of
infixr 9 **
infixl 8 * / rem
infixl 7 + -
infixr 7 ^
infixr 6 : ++
infixl 5 == <> < > <= >=
infixr 4 :=
infixr 3 &&
infixr 2 ||
infixl 0 |>
infixr 0 $

datatype bool = FALSE | TRUE
datatype (a)list = NIL | : (a, a list)
datatype (a)option = NONE | SOME a
datatype (a)ref = REF a

let write_file path text = write_file (path, text)
let sub s i = sub (s, i)
let substr s i n = substr (s, i, n)


# Booleans

let not TRUE  = FALSE
      | FALSE = TRUE

let foldbool f  _  TRUE  = f ()
           | _  f  FALSE = f ()

let mapbool f  TRUE  = f ()
          | _  FALSE = FALSE


# Characters

let islower c = let n = ord c in n >= ord 'a' && n <= ord 'z'
let isupper c = let n = ord c in n >= ord 'A' && n <= ord 'Z'
let isalpha c = islower c || isupper c
let isdigit c = let n = ord c in n >= ord '0' && n <= ord '9'
let isalnum c = isalpha c || isdigit c
let isspace c = c == ' ' || c == '\n' || c == '\t'


# Functions

let identity x = x
let const x _ = x
let flip f x y = f y x
let uncurry f (x, y) = f x y
let curry f x y = f(x, y)
let of(f, g) x = f (g x)
let |>(x, f) = f x
let $(f, x) = f x


# General

let pr x = print x; print "\n"; x
let fatal msg = pr msg; exit -1
let ! (REF value) = value
let equal x y = x == y
let not_equal x y = x <> y
let fst (x, _) = x
let snd (_, x) = x
let mapfst f (fst, snd) = (f fst, snd)
let mapsnd f (fst, snd) = (fst, f snd)
let pair x y = (x, y)
let true = const TRUE
let false = const FALSE


# Integer

let negate x = 0 - x
let abs x = if x < 0 then negate x else x
let min x y = if x < y then x else y
let max x y = if x > y then x else y
let pred x = x - 1
let succ x = x + 1


# Option
# foldopt - handle both cases
# bindopt - handle SOME value (function returns option)
# mapopt - handle SOME value (function returns normal value)

let issome (SOME _) = TRUE
         | _        = FALSE

let isnone NONE     = TRUE
         | _        = FALSE

let foldopt  f  _  (SOME x) = f x
          |  _  f  _        = f ()

let bindopt  f  (SOME x) = f x
          |  _  NONE     = NONE

let mapopt  f  (SOME x) = SOME (f x)
         |  f  NONE     = NONE

let default  x  NONE = x
          |  _  x    = x

let valof x = let (SOME x) = x in x

let opt  x  TRUE  = SOME x
      |  _  FALSE = NONE

let optof  f  TRUE  = SOME (f ())
        |  _  FALSE = NONE


#
# LIST
#

let foldl f leftmost list = let rec loop out (x : xs') = loop (f out x) xs'
                                       | out []        = out
                            in loop leftmost list

let foldl_self f (leftmost: list) = foldl f leftmost list

let reverse list = foldl (fn x y -> y : x) [] list

let foldr f rightmost list = foldl (flip f) rightmost (reverse list)

let foldr_self f list = let (rightmost : list') = reverse list in
                        foldl (fn x y -> f y x) rightmost list'

let append ([],  rhs) = rhs
         | (lhs, []) = lhs
         | (lhs, rhs) = foldr (curry (:)) rhs lhs

let ++ = append

let flatten lists = foldr (curry (++)) [] lists

let null [] = TRUE
       | _  = FALSE

let length list = let rec loop  n  (_ : xs) = loop (n + 1) xs
                             |  n  []       = n
                  in loop 0 list

let rec samelength  []     []    = TRUE
                 |  []     _     = FALSE
                 |  _      []    = FALSE
                 |  (_:x)  (_:y) = samelength x y

let tabulate n f =  let rec loop out i =
                      if i < n then loop (f i : out) (i + 1) else out
                    in reverse (loop [] 0)

let unfold f seed = let rec loop i out =
                      foldopt (fn (x, i') -> loop i' (x:out)) (fn _ -> reverse out) (f i)
                    in loop seed []

let hd x = let (x':_) = x in x'
let tl x = let (_:x') = x in x'
let cons hd tl = hd : tl
let singleton x = [x]
let rec last [x] = x
           | (_:x) = last x
           | [] = raise "#LAST"

let rec nth (x:_) 0 = x
          | (_:x) n = nth x (n - 1)

let map f list =  let rec loop out (x:xs) = loop (f x : out) xs
                             | out []     = out
                  in reverse (loop [] list)
let flatmap f list = flatten (map f list)
let mapi f list = let rec loop out i (x:xs) = loop (f i x : out) (i + 1) xs
                             | out _ []     = out
                  in reverse (loop [] 0 list)

let rec app  f  (x:xs) = f x; app f xs
          |  _  []     = ()

let rec find  p  (x:xs) = if p x then SOME x else find p xs
           |  _  []     = NONE

let rec findp  p  (x:xs) = foldopt SOME (fn _ -> findp p xs) (p x)
           |   _  []      = NONE

let rec any  p  (x:xs) = p x || any p xs
          |  _  []     = FALSE

let rec all  p  (x:xs) = p x && all p xs
          |  _  []     = TRUE

let none p list = not (any p list)

let assoc key list = mapopt snd (find (equal key of fst) list)

let filter p list = foldr (fn x xs -> if p x then x : xs else xs) [] list

let zip xs ys =
  let rec loop out (x:xs) (y:ys) = loop ((x, y):out) xs ys
             | out []     []     = reverse out
  in loop [] xs ys

let unzip zipped =
  let rec loop xs ys ((x, y):l) = loop (x:xs) (y:ys) l
             | xs ys []         = (reverse xs, reverse ys)
  in loop [] [] zipped



# STRING
let ^(lhs, rhs) = join [lhs, rhs]

let explode string =  let rec loop out i =
                        let c = sub string i in
                        if c == '\0' then out else loop (c : out) (i + 1)
                      in loop [] 0

let intersperse  _    []     = ""
              |  sep  (x:xs) = x ^ join(map (fn i -> sep ^ i) xs)

let split delim string =
  let rec loop i j out =
    let c = sub string j in
    if c == '\0'  then let field = substr string i (j - i) in
                       (field : out)
                  else
    if c == delim then let field = substr string i (j - i) in
                       loop (j + 1) (j + 1) (field : out)
                  else
    loop i (j + 1) out
  in
  reverse (loop 0 0 [])

let char_in_set set char =
  let rec loop i =
    let c = sub set i in
    if c == '\0' then FALSE else c == char || loop (i + 1)
  in
  loop 0

let find_char char i string =
  let rec loop i =
    let c = sub string i in
    if c == '\0'       then NONE
    else if c == char  then SOME i
    else loop (i + 1)
  in
  loop i

let unescape string =
  let rec loop i out =
    case find_char '\\' i string
    | NONE   -> substr string i (size string - i) : out
    | SOME j -> let out' = substr string i (j - i) : out
                and (c, i') = case sub string (j + 1)
                              | '\0' -> ("", j + 1)
                              | 'n'  -> ("\n", j + 2)
                              | 't'  -> ("\t", j + 2)
                              | c    -> (chrstr c, j + 2)
                in
                loop i' (c : out')
  in
  join (reverse (loop 0 []))

let escape quote string =
  let rec loop out i =
    case sub string i
    | '\0' -> out
    | '\\' -> loop ('\\':'\\':out) (i + 1)
    | '\n' -> loop ('n':'\\':out) (i + 1)
    | '\t' -> loop ('t':'\\':out) (i + 1)
    | c    -> loop (if quote == c then c:'\\':out else c:out) (i + 1)
  in
  implode (reverse (loop [] 0))

let escape_char c = escape '\'' (chrstr c)
let escape_string = escape '"'



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
    let c = sub text i in
    if isdigit c then
      loop (i + 1) (out * 10 + (ord c - ord '0'))
    else
      out
  in
  if sub text 0 == '-' then negate (loop 1 0) else
  if sub text 0 == '+' then loop 1 0 else
  loop 0 0



