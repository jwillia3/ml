infixl 4 -->
infixr 2 or

datatype token = TEOF | TERR | TINT | TCHR | TSTR | TCID | TID | TRESV
datatype type = | TYVAR (string, type option ref)
                | TYPE (type list, string)
datatype info = | LOC (string)
datatype exp =  | EINT (int, info)
                | ECHR (char, info)
                | ESTR (string, info)
                | EVAR (string, info)
                | ECON (string, info)
                | ETUP (exp list, info)
                | ELST (exp list, info)
                | EABS ((exp list, exp) list, info)
                | EAPP (exp, exp list, info)
                | EBIN (exp, exp, exp, info)
                | EIF (exp, exp, exp, info)
                | ECASE (exp, (exp, exp) list, info)
                | ELET (bool, (exp, exp) list, exp, info)
                | ESEQ (exp, exp, info)
                | ETY (exp, type, info)

let inttype = TYPE ([], "int")
let chartype = TYPE ([], "char")
let strtype = TYPE ([], "string")
let groundtypes = [("int", inttype),
                   ("char", chartype),
                   ("string", strtype)]

let prune (TYVAR (_, REF (SOME t))) = t
    |     t                         = t

let info (EINT (_,i))     = i
    |    (ECHR (_,i))     = i
    |    (ESTR (_,i))     = i
    |    (EVAR (_,i))     = i
    |    (ECON (_,i))     = i
    |    (ETUP (_,i))     = i
    |    (ELST (_,i))     = i
    |    (EABS (_,i))     = i
    |    (EAPP (_,_,i))   = i
    |    (EBIN (_,_,_,i)) = i
    |    (EIF (_,_,_,i))  = i
    |    (ECASE (_,_,i))  = i
    |    (ELET (_,_,_,i)) = i
    |    (ESEQ (_,_,i))   = i
    |    (ETY (_,_,i))    = i

let locof e = info e

datatype doc = | DT string        # Text fragment
               | DL               # Linebreak if vertical, space if horizontal
               | DG doc           # Group without increasing indent
               | DI doc           # Group and increase indent
               | DA doc           # Group and set indent to current column
               | DJ (doc, doc)    # Join two documents

let fmt width doc =
  let rec
  and spaces n = implode (tabulate n (const ' '))
  and loop (DT s,     l,  h,     r) = (s, r - size s)
      |    (DL,       l,  TRUE,  r) = (" ", r - 1)
      |    (DL,       l,  FALSE, _) = ("\n" ^ spaces l, width - l)
      |    (DG d,     l,  h,     r) = loop (d, l, widthof d <= r, r)
      |    (DI d,     l,  h,     r) = loop (d, l + 2, h || widthof d <= r, r)
      |    (DA d,     l,  h,     r) = loop (d, width - r, widthof d <= r, r)
      |    (DJ (a,b), l,  h,     r) = let (a', r')  = loop (a, l, h, r) in
                                      let (b', r'') = loop (b, l, h, r') in
                                      (a' ^ b', r'')
  and widthof (DT s)      = size s
      |       (DL)        = 1
      |       (DG d)      = widthof d
      |       (DI d)      = widthof d
      |       (DA d)      = widthof d
      |       (DJ (a, b)) = widthof a + widthof b

  in loop (doc, 0, widthof doc <= width, width)

# Pretty-Print Type
let pptype type =
  let rec
  and p (TYVAR (id, REF NONE))      = id
      | (TYVAR (_, REF (SOME t')))  = p t'
      | (TYPE ([x, y], "->"))       = join [paren x, "->", p y]
      | (TYPE (xs, ","))            = join ["(", intersperse "," (map p xs), ")"]
      | (TYPE ([], id))             = id
      | (TYPE ([t], id))            = join [paren t, " ", id]
      | (TYPE (ts, id))             = join [p (TYPE (ts, ",")), " ", id]
  and paren (TYPE ([x, y], "->")) = join ["(", p (TYPE ([x, y], "->")), ")"]
      |     t                     = p t
  in p type

# Pretty-Print Expression
let pp e =
  let

  and *(x, y) = DJ (x, y)

  and reduce _   f []     = DT ""
      |      sep f (x:xs) = foldl (fn a b-> a * sep * b) (f x) (map f xs)
  in

  let rec
  and csv xs = reduce (DT "," * DL) p xs

  and decl (f, EABS (cs,_)) = p f * DT " " * clauses " =" cs
      |    (x, y)           = DI (paren x * DT " = " * p y)

  and rule (x, y) = DI (DT "| " * paren x * DT " ->" * DL * p y)

  and clauses delim cs = DA (reduce (DL * DT "| ") (clause delim) cs)
  and clause d (xs, e) =  let e' = case e
                                   | (ECASE e) -> paren (ECASE e)
                                   | _         -> p e
                          in
                          DG (reduce (DT " ") paren xs * DT d * DL * e')

  and p (EINT (x,_))        = DT (itoa x)
      | (ECHR (x,_))        = DT "'" * DT (escape_char x) * DT "'"
      | (ESTR (x,_))        = DT "\"" * DT (escape_string x) * DT "\""
      | (EVAR (x,_))        = DT x
      | (ECON (x,_))        = DT x
      | (ETUP (x,_))        = DT "(" * DA (csv x) * DT ")"
      | (ELST (x,_))        = DT "[" * DA (csv x) * DT "]"
      | (EABS (cs,_))       = DT "fn " * clauses " ->" cs
      | (EAPP (f,xs,_))     = p f * DT " " * DA (reduce DL paren xs)
      | (EBIN (l,o,r,_))    = paren l * DT " " * p o * DT " " * paren r
      | (EIF (c,t,f,_))     = DT "if " * DI (paren c) * DT " then" *
                              DI (DL * paren t) *
                              DL * DT "else" *
                              DI (DL * paren f)
      | (ECASE (e,r,_))     = DT "case " * paren e * DL * reduce (DL) rule r
      | (ELET (r,d,e,_))    = DT (if r then "let rec" else "let") * DT " " *
                              DG (reduce (DL * DT "and ") decl d *
                                  DL * DT "in") *
                              DL * p e
      | (ESEQ (a,b,_))      = DG (paren a * DT ";" * DL * paren b)
      | (ETY (e,t,_))       = DG (paren e * DT "::" * DT (pptype t))

  and paren
      |     (EAPP x)  = DT "(" * DA (p (EAPP x)) * DT ")"
      |     (EBIN x)  = DT "(" * DA (p (EBIN x)) * DT ")"
      |     (ELET x)  = DT "(" * DA (p (ELET x)) * DT ")"
      |     (ECASE x) = DT "(" * DA (p (ECASE x)) * DT ")"
      |     (ETY x)   = DT "(" * DA (p (ETY x)) * DT ")"
      |     e         = p e

  in
  fst (fmt 80 (p e))


let error loc msg =
  pr (join ["ERROR ", loc, ": ", msg]);
  exit 1



# Convert source into tokens

let tokenise path src =
  let

  # Primitives
  # ----------
  # Combinators take a src index and returns a new index after consumed chars.
  # NONE is returned if no character was consumed.
  # `a` consumes a single char that matches the given predicate
  # `lit` consumes the given char
  # `many` runs its combinator sequentially until the point it returns NONE
  # `many1` is the same as `many` except it must succeed at least once
  # `*` runs the left combinator followed by the right
  # `-->` runs the left combinator and transforms the result with the right
  # `or` runs the left combinator or the right if the left returns NONE

  and -->(p, f) i = mapopt (fn i' -> f i' (substr src i (i' - i))) (p i)
  and *(a, b) i = bindopt b (a i)
  and or(a, b) i = foldopt SOME (fn _ -> b i) (a i)
  and err msg i _ = (TERR, msg, i)

  and a p i = let c = sub src i in opt (i + 1) (c <> '\0' && p c)
  and lit c = a (equal c)
  and many p i = let rec loop i = foldopt loop (fn _ -> SOME i) (p i) in loop i
  and many1 p = p * many p

  and type type j text = (type, text, j)
  and pun = char_in_set "()[],;"
  and idchr c =  isalnum c || c == '_' || c == '\''
  and opchr = char_in_set "!%&$*+-/:<=>?@~`^|"
  and resvd = ["=","let","rec","and","in","->","case","|","if","then","else",
              "infixl","infixr","datatype","::","fn"]
  and idtype i' id = if any (equal id) resvd then (TRESV, id, i') else
                     if id == ":" then (TCID, id, i') else
                     if isupper (sub id 0) then (TCID, id, i') else
                     (TID, id, i')

  and character quote = (lit '\\' * a true) or a (fn c-> c<>quote && c<>'\n')
  and chartype type i' text = (type, unescape (substr text 1 (size text - 2)), i')

  and token =
    a pun --> type TRESV
    or many1 (a isdigit) --> type TINT
    or lit '+' * many1 (a isdigit) --> type TINT
    or lit '-' * many1 (a isdigit) --> type TINT
    or lit '\'' * character '\'' * lit '\'' --> chartype TCHR
    or lit '"' * many (character '"') * lit '"' --> chartype TSTR
    or lit '\'' --> err "character not closed"
    or lit '"' --> err "string not closed"
    or many1 (a idchr) --> idtype
    or many1 (a opchr) --> idtype

  in

  # Read all tokens including `TEOF` token.
  # Each run of the loop keeps track of the current index, `i`,
  # the index of the start of the line, `sol`, and the line number, `ln`.
  let rec loop i sol ln out =
    let loc = join [path, ":", itoa ln, ":", itoa (i - sol + 1)] in

    if i >= size src then
      reverse ((TEOF, "", loc) : out)

    else
      let c = sub src i in
      if c == ' '  then loop (i + 1) sol ln out else
      if c == '\t' then loop (i + 1) sol ln out else
      if c == '\n' then loop (i + 1) (i + 1) (ln + 1) out else
      if c == '#'  then loop (valof (many (a (not_equal '\n')) i)) sol ln out else

      case token i
      | NONE                  -> error loc ("lexical error: " ^ chrstr c)
      | SOME (TERR, msg, i')  -> error loc msg
      | SOME (type, text, i') -> loop i' sol ln ((type, text, loc) : out)
  in
  loop 0 0 1 []


let rec parse (types, cons) tokens =
  let
  and src = REF tokens
  and unique = let id = REF 0 in fn () -> id := !id + 1
  and infixes = REF [] :: (string, (int, int)) list ref
  and types = REF types :: (string, type) list ref
  and cons = REF cons :: (string, type) list ref
  and syntax msg = let ((_,_,l):_) = !src in error l msg
  and loc () = let ((_,_,l):_) = !src in LOC l
  and next () = let ((_,t,_):_) = !src in src := tl (!src); t
  and kind k = let ((k',_,_):_) = !src in k==k'
  and needkind k what = if kind k then next () else syntax ("need " ^ what)
  and lit t = let ((k,t',_):_) = !src in k==TRESV && t==t' && (next (); TRUE)
  and req t = lit t || syntax ("need " ^ t)
  and plist sep p =
    let rec loop out = (if lit sep then loop (p () : out) else reverse out) in
    lit sep; p () : loop []
  and list p =
    let rec loop out (SOME x) = loop (x:out) (p ())
            |    out _        = reverse out
    in
    loop [] (p ())
  and csv p delim =
    let rec loop out = if lit delim then reverse out else
                       let x = p () in
                       if lit "," then loop (x:out) else
                       req delim; reverse (x:out)
    in loop []

  # Determine if the next token is an infix on level n (or n is negative)
  and infix n =
    let ((k,t,_):_) = !src in
    if k==TID || k==TCID then
      case assoc t (!infixes)
      | SOME (lhs, rhs) ->  if lhs==n || n<0 then
                              let f = if k==TID then EVAR else ECON in
                              SOME (f, t, rhs)
                            else
                              NONE
      | NONE            -> NONE
    else
      NONE
  in

  # Recursive Descent Parser
  let rec

  and top () =

    let rec loop out =

      if lit "datatype" then
        let ps' = if lit "(" then csv (fn ()-> needkind TID "ID") ")" else []
        and ps = map (fn id-> TYVAR (id, REF NONE)) ps'
        and id = needkind TID "type ID"
        in

        if issome (assoc id (!types)) then
          syntax ("type already defined: " ^ id)
        else

        let type = TYPE (ps, id)
        and types' = (id, type):(!types)
        and cdecl () =
          let id = needkind TCID "constructor ID" in
          if issome (assoc id (!cons)) then
            syntax ("constructor already defined: " ^ id)
          else
          let ctype = case type' ()
                      | SOME x -> TYPE ([x, type], "->")
                      | NONE   -> type
          in
          cons := (id, ctype):(!cons)
        in

        types := zip ps' ps ++ types';  # Temporarily add type params
        req "="; plist "|" cdecl;
        types := types';                # Remove type params

        loop out
      else

      if lit "let" then
        let at = loc () in
        loop ((lit "rec", decls (), at):out)
      else

      if lit "infixl" then idecl 1; loop out else
      if lit "infixr" then idecl 0; loop out else

      if kind TEOF then reverse out else

      syntax "need top-level decl"
    in
    let defs = loop [] in
    (!types, !cons, defs)

  and idecl adjust =
    let lhs = atoi (needkind TINT "operator level")
    and rhs = lhs + adjust
    and ops = list (fn ()-> optof next (kind TID || kind TCID))
    in
    infixes := foldl (fn old id-> (id, (lhs, rhs)):old) (!infixes) ops

  and type' () = case !src
                 | ((TID, _, _):_)     -> SOME (type ())
                 | ((TRESV, "(", _):_) -> SOME (type ())
                 | _                   -> NONE

  and type () = let lhs = btype () in
                if lit "->" then TYPE ([lhs, type ()], "->") else
                lhs

  and btype () =  # Decompose a tuple arg to satisfy multi-arg type constructor.
                  let multiple at arg params typeid =
                    let exec xs =
                      if samelength params xs then TYPE (xs, typeid) else
                      error at (join ["wrong arity: ", typeid, " ",
                                      itoa (length xs), "/",
                                      itoa (length params)])
                    in
                    case prune arg
                    | TYPE (xs, ",") -> exec xs
                    | t              -> exec [t]

                  and apply at lhs tycon =
                    case tycon
                      | TYPE ([], id)  -> error at ("type takes no args: " ^ id)
                      | TYPE ([_], id) -> TYPE ([lhs], id)
                      | TYPE (ps, id)  -> multiple at lhs ps id
                      | t              -> error at ("not typecon: " ^ pptype t)
                  in

                  let rec loop lhs =
                    let (LOC at) = loc () in
                    if kind TID then loop (apply at lhs (typeref ())) else lhs
                  in
                  loop (atype ())

  and atype () =  if kind TID then
                    let (LOC at) = loc () in
                    case typeref ()
                    | TYPE (_:_, id) -> error at ("type requires args: " ^ id)
                    | t              -> t
                  else
                  if lit "(" then
                    let ts = csv type ")" in
                    if length ts == 1 then hd ts else
                    TYPE (ts, ",")
                  else
                  syntax "need type"

  and typeref () =  let (LOC at) = loc () in
                    let id = next () in
                    case assoc id (!types)
                    | SOME type -> prune type
                    | NONE      -> error at ("undefined type: " ^ id)

  and decls () = plist "and" (fn() -> (aexp (), function "="))

  and exp () =
    let at = loc () in
    let x = lexp () in
    if lit ";" then ESEQ (x, exp (), at) else
    if lit "::" then ETY (x, type (), at) else
    x

  and lexp () =
    let at = loc () in
    if lit "if" then
      EIF (exp (), req "then"; exp (), req "else"; exp (), at)
    else
    if lit "case" then
      let rule () = if lit "|" then SOME (exp (), req "->"; exp ()) else NONE in
      ECASE (exp (), list rule, at)
    else
    if lit "let" then
      ELET (lit "rec", decls (), req "in"; exp (), at)
    else
    iexp 0

  and iexp n =
    if n == 11 then appexp () else
    let rec loop lhs =
      case infix n
      | NONE              ->  lhs
      | SOME (f, id, n')  ->  let at = locof lhs in
                              let rhs = next (); iexp n' in
                              let op = f (id, at) in
                              loop (EBIN (lhs, op, rhs, at))
    in loop (iexp (n + 1))

  and appexp () =
    let (f, xs) = (aexp (), list arg) in
    if xs == [] then f else EAPP (f, xs, locof f)

  and arg () = if issome (infix -1) then NONE else aexp' ()

  and aexp' () =
    let at = loc () in
    if kind TINT  then SOME (EINT (atoi (next ()), at)) else
    if kind TCHR  then SOME (ECHR (sub (next ()) 0, at)) else
    if kind TSTR  then SOME (ESTR (next (), at)) else
    if kind TID   then SOME (EVAR (next (), at)) else
    if kind TCID  then SOME (ECON (next (), at)) else
    if lit "("   then SOME (ETUP (csv exp ")", at)) else
    if lit "["   then SOME (ELST (csv exp "]", at)) else
    if lit "fn"  then SOME (function "->") else
    NONE

  and aexp () = foldopt identity (fn _-> syntax "need expression") (aexp' ())

  and function delim =
    if lit delim then exp () else
    let at = loc () in
    EABS (clauses delim, at)

  and clauses delim = plist "|" (fn()-> (list aexp', req delim; exp ()))
  in
  top ()

let defexp defs =
  let dummy = ETUP ([], LOC "")
  and combine (FALSE,a,at) (ELET (FALSE,b,e,_)) = ELET (FALSE, a++b, e, at)
      |       (r, ds, at)  e                    = ELET (r, ds, e, at)
  in
  foldr combine dummy defs

let readsourcefile (types, cons, defs) path =
  case read_file path
  | NONE -> error path "cannot open source"
  | SOME text ->  let tokens = tokenise path text in
                  let (types, cons, defs') = parse (types, cons) tokens in
                  (types, cons, defs ++ defs')

let (types, cons, defs) = (groundtypes, [], [])
let (types, cons, defs) = readsourcefile (types, cons, defs) "boot.ml"
let (types, cons, defs) = readsourcefile (types, cons, defs) "ml.ml"
# let (types, cons, defs) = readsourcefile (types, cons, defs) "test.ml"
let e = defexp defs
# let _ = pr (pp $ e)

