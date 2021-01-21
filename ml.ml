infixl 6 -->
infixr 2 or
datatype token = TEOF | TINT | TCHAR | TSTRING | TID | TCID | TRESV
datatype type = TYPE(type list, string) | TYPEVAR(string, type option ref)
datatype misc = | LOC string
datatype exp = | EINT(int, misc list)
               | ECHAR(char, misc list)
               | ESTRING(string, misc list)
               | EVAR(string, misc list)
               | ECON(string, misc list)
               | ETUPLE(exp list, misc list)
               | EFN(string, exp, misc list)
               | EAPP(exp, exp, misc list)
               | ECASE(exp, (exp, exp) list, misc list)
               | ELET(exp, exp, exp, misc list)
               | EREC((string, exp) list, exp, misc list)
               | ETYPING(exp, type, misc list)
               | ESEQ(exp, exp, misc list)
               | EAS(string, exp, misc list)

let fatal loc msg = pr (join ["ERROR ", loc, ": ", msg]); exit 1



#
# Types. ---------------------------------------------------------------
#

let int_type = TYPE([], "int")
let string_type = TYPE([], "string")

let fn_type x y = TYPE([x, y], "->")
let tuple_type xs = TYPE(xs, ",")

let rec
and target(TYPEVAR(_, REF(SOME t))) = t
--- target t                        = t

# Pretty-Print type.
let rec ppt :: type -> string
--- ppt(TYPE(xs,","))     = join ["(", intersperse ", " (map ppt xs), ")"]
--- ppt(TYPE([a,b],"->")) = join ["(", ppt a, "->", ppt b, ")"]
--- ppt(TYPE([],id))      = id
--- ppt(TYPE([x],id))     = join [ppt x, " ", id]
--- ppt(TYPE(xs,id))      = join ["(", intersperse ", " (map ppt xs), ") ", id]
--- ppt(TYPEVAR(_,REF(SOME t))) = ppt t
--- ppt(TYPEVAR(id,_))    = id




#
# Expressions. ---------------------------------------------------------
#


# Pretty-Print expression.
let rec pp :: exp -> string
--- pp(EINT(i,_))     = itoa i
--- pp(ECHAR(c,_))    = join ["'", escape_char (chrstr c), "'"]
--- pp(ESTRING(s,_))  = join ["\"", escape_string s, "\""]
--- pp(EVAR(id,_))    = id
--- pp(ECON(id,_))    = id
--- pp(ETUPLE(xs,_))  = join ["(", intersperse ", " (map pp xs), ")"]
--- pp(EFN(p,b,_))    = join ["(fn ", p, " -> ", pp b, ")"]
--- pp(EAPP(f,x,_))   = join ["(", pp f, " ", pp x, ")"]
--- pp(ECASE(x,rs,_)) = let f(l,r) = [" | ", pp l, " -> ", pp r] in
                        let rs' = join (flatmap f rs) in
                        join (["(case ", pp x, rs', ")"])
--- pp(ELET(x,e,m,_)) = join ["(let ", pp x, " = ", pp e, " in ", pp m, ")"]
--- pp(EREC(ds,m,_))  = let f(l,r) = join [l, " = ", pp r] in
                        let ds' = intersperse " and " (map f ds) in
                        join ["(let rec ", ds', " in ", pp m, ")"]
--- pp(ETYPING(x,t,_)) = join ["(", pp x, " :: ", ppt t, ")"]
--- pp(ESEQ(x,y,_))   = join ["(", pp x, "; ", pp y, ")"]
--- pp(EAS(id,x,_))   = join ["(", id, " @ ", pp x, ")"]

# Get attributes of an expression.
let rec
and misc_of(EINT(_,x)) = x
--- misc_of(ECHAR(_,x)) = x
--- misc_of(ESTRING(_,x)) = x
--- misc_of(EVAR(_,x)) = x
--- misc_of(ECON(_,x)) = x
--- misc_of(ETUPLE(_,x)) = x
--- misc_of(EFN(_,_,x)) = x
--- misc_of(EAPP(_,_,x)) = x
--- misc_of(ECASE(_,_,x)) = x
--- misc_of(ELET(_,_,_,x)) = x
--- misc_of(EREC(_,_,x)) = x
--- misc_of(ETYPING(_,_,x)) = x
--- misc_of(ESEQ(_,_,x)) = x
--- misc_of(EAS(_,_,x)) = x

let rec
and is_fn_exp(EFN _)          = TRUE
--- is_fn_exp(ETYPING(x,_,_)) = is_fn_exp x
--- is_fn_exp _               = FALSE

let locate e = fold_option (fn(LOC x) -> x)
                           (fn() -> "unknown:0:0")
                           (find (fn(LOC x) -> TRUE | _ -> FALSE) (misc_of e))


#
# Parsing. -------------------------------------------------------------
#

let tokenise filename src =
  let
  and *(p, f) i = bind_option f (p i)
  and or(a, b) i = fold_option SOME (fn() -> b i) (a i)
  and p f i = let c = sub(src, i) in option_of (i + 1) (c <> '\0' && f c)
  and p0' f = let rec loop i = fold_option loop (const (SOME i)) (f i) in loop
  and p0 f = p0' (p f)
  and p1 f = p f * p0 f
  and l c i = p (equal c) i
  and -->(p, f) i = map_option (fn i' -> f i' (substring(src, i, i' - i))) (p i)
  and type type i' text = (type, text, i')
  and resv = ["=", "let", "rec", "and", "in", "fn", "->",
              "case", "|", "if", "then", "else", "infixl",
              "infixr", "datatype", "::", "---"]
  and classify i id = if isupper(sub(id, 0)) then (TCID, id, i) else
                      if id == ":" then (TCID, id, i) else
                      if any (equal id) resv then (TRESV, id, i) else
                      (TID, id, i)
  and pun = char_in_set "()[],;"
  and id c = isalnum c || c == '_' || c == '\''
  and sym = char_in_set "!%&$*+-/:<=>?@\~`^|"
  and ctype type i s = (type, unescape (substring(s, 1, size s - 2)), i)
  and char quote = l '\\' * p (not_equal '\0') or p (not_equal quote)
  and token = p1 isdigit                        --> type TINT
              or l '-' * p1 isdigit             --> type TINT
              or l '+' * p1 isdigit             --> type TINT
              or p pun                          --> type TRESV
              or l '\'' * char '\'' * l '\''    --> ctype TCHAR
              or l '"' * p0' (char '"') * l '"' --> ctype TSTRING
              or p1 sym                         --> classify
              or p1 id                          --> classify
  in
  let rec loop line sol i out =
    let loc() = join [filename, ":", itoa line, ":", itoa (i - sol + 1)] in
    let c = sub(src, i) in
    if c == '\0' then (TEOF, "", loc()) : out else
    if c == '\n' then loop (line + 1) (i + 1) (i + 1) out else
    if isspace c then loop line sol (i + 1) out else
    if c == '#'  then loop line sol (value_of(p0 (not_equal '\n') i)) out else
    case token i
    | SOME(type, text, i') -> loop line sol i' ((type, text, loc()) : out)
    | NONE                 -> fatal (loc()) "bad token"
  in
  reverse (loop 1 0 0 [])

let parse :: (string, string, string) list -> exp
--- parse tokens =

  # Tables.
  let
  and inftab = REF [] :: (string, (int, int)) list ref
  and typetab = REF [("int", int_type), ("string", string_type)] :: (string, type) list ref
  and contab = REF [] :: (string, type) list ref
  and toplist = REF [] :: exp list ref
  in

  # Token stream.
  let src = REF tokens
  and toktext = REF ""
  and tokloc = REF ""
  and nexttext() = let ((_,x,_):_) = !src in x
  and nextloc() = let ((_,_,x):_) = !src in x
  and next() = let ((_,t,l):src') = !src in
                toktext := t;
                tokloc := l;
                src := src';
                TRUE
  and peek k = let ((k',_,_):src') = !src in k == k'
  and kind k = peek k && next()
  and want t = peek TRESV && nexttext() == t && next()
  and err msg = fatal (nextloc()) msg
  and need t = want t || err ("need " ^ t)
  and next_op level =
    if peek TID || peek TCID then
      case assoc (nexttext()) (!inftab)
      | NONE      -> NONE
      | SOME(l,r) -> if l==level || level<0 then SOME(l,r) else NONE
    else NONE
  in

  # Helpers.
  let
  and app f x = EAPP(f, x, [LOC $ locate x])
  and bin misc f x y = EAPP(f, ETUPLE([x, y], misc), misc)
  and tuple misc [x] = x
  --- tuple misc xs  = ETUPLE(xs, misc)
  and dummy = ECON("-DUMMY-", [])
  in

  let rec
  and need_kind k what = if kind k then !toktext else err ("need " ^ what)
  and need_id() = need_kind TID "id"
  and seq item delim =
    if want delim then [] else
    item() : (if want "," then seq item delim else need delim; [])
  and many item =
    fold_option (fn x -> x : many item)
                (fn() -> [])
                (item())
  and pmany p item = if p() then item() : pmany p item else []
  and pmany1 p item = item() : pmany p item
  in

  let rec

  and top() =
    if peek TEOF then fold_top (!toplist) else
    if want "datatype" then datbind(); top() else
    if want "infixl" then infixdec 1; top() else
    if want "infixr" then infixdec 0; top() else
    if want "let" then top_let(); top() else
    err "need top-level declaration"

  and datbind() =
    let (args, id)  = if want "(" then
                        (seq need_id ")", need_id())
                      else
                        let tmp = need_id() in
                        if peek TID then ([tmp], need_id()) else ([], tmp)
    and argmap      = map (fn i -> (i, TYPEVAR(i, REF NONE))) args
    and type        = TYPE(map snd argmap, id)
    and typetab'    = typetab := (id, type) : !typetab
    in
    need "=";
    typetab := argmap ++ !typetab;
    pmany1 (fn() -> want "|") (conbind type);
    typetab := typetab'

  and conbind basetype () =
    if not (kind TCID) then err "need constructor id" else
    let loc = !tokloc
    and id = !toktext
    and type = case ty_opt()
               | SOME(x) -> fn_type x basetype
               | NONE    -> basetype
    in
    if assoc id (!contab) |> is_some then fatal loc (id ^ " already defined") else
    contab := (id, type) : !contab

  and infixdec adjust =
    let loc = nextloc()
    and symbol() = if kind TID || kind TCID then SOME(!toktext) else NONE
    and (level, ids) = (need_kind TINT "level" |> atoi, many symbol)
    in
    if level < 0 || level > 10 then fatal loc "level must be 0 - 10" else
    inftab := map (fn id -> (id, (level, level + adjust))) ids ++ !inftab

  and top_let() =
    let misc = [LOC $ nextloc()] in
    toplist := let_dec misc (const dummy) : !toplist

  and let_dec misc body' =
    let is_rec = want "rec" in
    let (ds, body) = (pmany1 (fn() -> want "and") dec, body'()) in

    if is_rec then
      case find (fn(EVAR _)->FALSE | _->TRUE) (map fst ds)
      | SOME(lhs) -> fatal (locate lhs) "let rec l.h.s. must be id"
      | NONE      -> ();

      case find (not of is_fn_exp) (map snd ds)
      | SOME(rhs) -> fatal (locate rhs) "let rec r.h.s. must be function"
      | NONE      -> ();

      let ds' = map (fn(EVAR(id,_),r) -> (id,r)) ds in
      EREC(ds', body, misc)
    else
      foldr (fn (l,r) b -> ELET(l,r,b,misc)) body ds

  and dec() =
    if peek TID then
      let id = next(); !toktext
      and misc = [LOC $ !tokloc]
      and typing = if want "::" then SOME(ty_with_coining()) else NONE
      and clause() = (many atexp_opt, need "="; exp())
      and cont() = if want "---" then
                    if nexttext() <> id || not (kind TID) then
                      err ("must repeat id after ---: " ^ id)
                    else TRUE
                  else FALSE
      and clauses = pmany1 cont clause
      and (l, r) = (EVAR(id, misc), desugar_fn misc clauses)
      in
      case typing
      | SOME(t) -> (l, ETYPING(r, t, misc))
      | NONE    -> (l, r)

    else
      (atexp(), need "="; exp())

  and exp() =
    let rec loop lhs =
      if want "::" then loop $ ETYPING(lhs, ty_with_coining(), misc_of lhs) else
      if want ";" then loop $ ESEQ(lhs, exp(), misc_of lhs) else
      lhs
    in
    loop (lexp())

  and lexp() =
    let misc = [LOC $ nextloc()] in

    if want "if" then
      let (a,b,c) = (exp(), need "then"; exp(), need "else"; exp()) in
      let b' = (ECON("TRUE", misc_of b), b) in
      let c' = (ECON("FALSE", misc_of c), c) in
      ECASE(a, [b', c'], misc)
    else
    if want "case" then
      let rule() = (exp(), need "->"; exp()) in
      ECASE(exp(), pmany (fn() -> want "|") rule, misc)
    else
    if want "let" then
      let_dec misc (fn() -> need "in"; exp())
    else
      infexp 0

  and infexp level =
    if level==11 then
      appexp()
    else
      let rec loop lhs =
        case next_op level
        | NONE      -> lhs
        | SOME(_,r) -> let misc = misc_of lhs
                       and f' = if kind TCID then ECON else (kind TID; EVAR)
                       and f = f'(!toktext, misc)
                       in
                       loop (bin misc f lhs (infexp r))
      in
      loop (infexp (level + 1))

  and appexp() = foldl_self app (atexp() : many atexp_opt)

  and atexp() = value_of(atexp' TRUE)
  and atexp_opt() = atexp' FALSE
  and atexp' required =
    # Avoid consuming operator as argument.
    if not required && is_some(next_op -1) then NONE else

    let misc = [LOC $ nextloc()] in
    if kind TINT    then SOME $ EINT(!toktext |> atoi, misc) else
    if kind TCHAR   then SOME $ ECHAR(sub(!toktext, 0), misc) else
    if kind TSTRING then SOME $ ESTRING(!toktext, misc) else
    if kind TID     then SOME $ EVAR(!toktext, misc) else
    if kind TCID    then SOME $ ECON(!toktext, misc) else
    if want "("     then SOME $ tuple misc (seq exp ")") else
    if want "["     then let f = ECON(":", misc) in
                         SOME $ foldr (bin misc f)
                                      (ECON("NIL", misc))
                                      (seq exp "]")
    else
    if want "fn" then
      let typing = if want "::" then SOME $ ty_with_coining() else NONE
      and clause() = (many atexp_opt, need "->"; exp())
      and clauses = pmany1 (fn() -> want "|") clause
      and value = desugar_fn misc clauses
      in
      case typing
      | SOME(t) -> SOME(ETYPING(value, t, misc))
      | NONE    -> SOME(value)
    else
    if not required then
      NONE
    else
      err "need expression"

  and ty_opt() =
    if peek TID || (peek TRESV && nexttext() == "(") then
      SOME $ ty()
    else
      NONE
  and ty() = ty' FALSE
  and ty_with_coining() =
    let old_typetab = !typetab in
    let out = ty' TRUE in
    typetab := old_typetab;
    out


  and ty' coining =
    let t = appty coining in
    if want "->" then
      fn_type t (ty' coining)
    else
      t

  and appty coining =
    let rec loop t =
      if kind TID then
        let (id, loc) = (!toktext, !tokloc) in
        loop $ apply_typecon id loc t $ map_option target (assoc id (!typetab))
      else
        t
    in
    loop (atty coining)

  and atty coining =
    if want "(" then
      tuple_type(seq (fn() -> ty' coining) ")")
    else
    if kind TID then
      let (id, loc) = (!toktext, !tokloc) in
      case map_option target (assoc id (!typetab))
      | SOME t @ (TYPE([],_))  -> t
      | SOME(TYPE(_,_))        -> fatal loc ("type needs args: " ^ id)
      | SOME t @ (TYPEVAR _)   -> target t
      | NONE                   -> if coining then
                                    let t = TYPEVAR(id, REF NONE) in
                                    typetab := (id, t) : !typetab;
                                    t
                                  else
                                    fatal loc ("undefined type: " ^ id)
    else
      err "need type"



  and apply_typecon loc id arg type =
    case type
    | SOME(TYPE([],_))  -> fatal loc ("type takes no args: " ^ id)
    | SOME(TYPE(ps,id)) -> (case (ps,arg)
                            | ([x], arg) -> TYPE([arg], id)
                            | (xs, TYPE(ys,",")) ->
                              if same_length xs ys then
                                 TYPE(ys,id)
                              else
                                 fatal loc ("wrong number of args: " ^ id))
    | SOME(TYPEVAR _)   -> fatal loc ("typevar cannot take args: " ^ id)
    | NONE              -> fatal loc ("undefined type: " ^ id)

  and desugar_fn misc clauses =
    let id_of(EVAR(id,_)) = SOME id
    --- id_of _           = NONE
    and is_simple e = id_of e |> is_some
    in

    let n = length $ fst $ hd clauses in

    if any (not_equal n) (map (length of fst) clauses) then
      err "not all clauses agree in arity"
    else

    # If there's only one clause and its params are all vars, use simple fn.
    # Otherwise, make simple fn and use case analysis.
    if length clauses == 1 && all is_simple (fst $ hd clauses) then
      foldr (fn p b -> EFN(p, b, misc))
            (snd $ hd clauses)
            (map (value_of of id_of) (fst $ hd clauses))
    else
      let pars = tabulate n (fn i -> "'" ^ itoa i)
      and subject = tuple misc (map (fn id -> EVAR(id, misc)) pars)
      and rules = map (fn(l, r) -> (tuple (misc_of $ hd l) l, r)) clauses
      and body = ECASE(subject, rules, misc)
      in
      foldr (fn p b -> EFN(p, b, misc)) body pars

  and fold_top exps =
    let rec
    and unfold (ELET(l,r,x@(ELET _),m)) = [ELET(l,r,dummy,m)] ++ unfold x
    --- unfold x = [x]
    in

    let
    and combine b (EREC(ds,_,m)) = EREC(ds, b, m)
    --- combine b (ELET(l,r,_,m)) = ELET(l,r,b,m)
    in
    foldl combine (ETUPLE([], [])) (flatmap unfold exps)
  in
  top()



let filename = "boot.ml"
let filename = "test.ml"
let src = value_of(read_file filename)
let tokens = tokenise filename src
let _ = pr $ pp $ parse tokens
let _ = pr "ok."
