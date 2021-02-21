infixl 6 -->
infixr 2 or

datatype token_type = TINT | TCHR | TSTR | TCID | TID | TRESV
datatype misc = MISCLOC string
datatype type =
  | TVAR (string, type option ref)
  | TYPE (string, type list)
  | TFN (type, type)
  | TTUPLE (type list)
datatype exp =
  | EINT (int, misc)
  | ECHR (char, misc)
  | ESTR (string, misc)
  | EAT (string, exp, misc)
  | EVAR (string, misc)
  | ECON (string, misc)
  | ETUPLE (exp list, misc)
  | ELIST (exp list, misc)
  | EFN (exp list, exp, misc)
  | EAPP (exp, exp, misc)
  | ECASE (exp, (exp, exp) list, misc)
  | ELET (bool, exp, (exp, exp) list, misc)
  | ESEQ (exp, exp, misc)
  | ETYPE (exp, type, misc)


let error loc msg =
  pr (join ["error ", loc, ": ", msg]);
  exit 1

let tokenise path src =
  let
  and -->(parse, xform) i = mapopt (\j -> xform j (substr src i (j - i))) (parse i)
  and *(a, b) i = bindopt b (a i)
  and or(a, b) i = foldopt SOME (\_ -> b i) (a i)

  and a p i = let c = sub src i in opt (i + 1) (c <> '\0' && p c)
  and lit c = a (equal c)
  and many p i = let rec loop i = foldopt loop (\_->SOME i) (p i) in loop i
  and many1 p = p * many p

  and type type j text = (type, text, j)
  and pun = char_in_set "()[],;\\"
  and idchr c =  isalnum c || c == '_' || c == '\''
  and opchr = char_in_set "!%&$*+-/:<=>?@~`^|"
  and resvd = ["=","let","rec","and","in","->","case","|","if","then","else",
              "infixl","infixr","datatype","::","---","@"]
  and idtype i' id = if any (equal id) resvd then (TRESV, id, i') else
                     if isupper (sub id 0) then (TCID, id, i') else
                     (TID, id, i')

  and character quote = (lit '\\' * a true) or a (not_equal quote)
  and chartype type i' text = (type, unescape (substr text 1 (size text - 2)), i')

  and token =
    a pun --> type TRESV
    or many1 (a isdigit) --> type TINT
    or lit '+' * many1 (a isdigit) --> type TINT
    or lit '-' * many1 (a isdigit) --> type TINT
    or lit '\'' * character '\'' * lit '\'' --> chartype TCHR
    or lit '"' * many (character '"') * lit '"' --> chartype TSTR
    or many1 (a idchr) --> idtype
    or many1 (a opchr) --> idtype

  in
  let rec loop i sol ln out =
    let loc = join [path, ":", itoa ln, ":", itoa (i - sol + 1)] in
    if i >= size src then out else
    let c = sub src i in
    if c == ' '  then loop (i + 1) sol ln out else
    if c == '\t' then loop (i + 1) sol ln out else
    if c == '\n' then loop (i + 1) (i + 1) (ln + 1) out else
    if c == '#'  then loop (val $ many (a (not_equal '\n')) i) sol ln out else
    case token i
    | SOME (type, text, i') -> loop i' sol ln ((type, text, loc) : out)
    | NONE -> error loc ("lexical error: " ^ chrstr c)
  in
  reverse (loop 0 0 1 [])

let parse src =
  let
  and *(p, f) src = bindopt (uncurry f) (p src)
  and or(a, b) src = foldopt SOME (\_ -> b src) (a src)
  and ret x src' = (x, src')


  and kind
  --- k ((k',txt,_):src') = opt (txt, src') (k==k')
  --- _ _                 = NONE

  and lit
  --- t ((TRESV,t',_):src') = opt ((), src') (t==t')
  --- _ _                   = NONE


  in

  let rec exp src = (lit "datatype" * \_ -> kind TID) src
  in
  exp src

let source = "abc9\n123,"
let source = read_file "test.ml" |> val
let tokens = tokenise "path" source
let _ = pr (parse tokens)
