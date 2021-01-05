Synopsis
--------


Grammar
-------
```
top:        ("datatype" datbind | infixdec | "let" ["rec"] ["and"] dec )
datbind:    (id | "(" id_seq ")") id "=" ty | condbind
conbind:    ["|"] id [ty] {"|" id [ty]}
infixdec:   "infixl"/"infixr" int {id/cid}
ty:         ty {'->' ty}
appty:      ty {id}
atty:       id / "(" ty_seq ")"
dec:        exp "=" exp
exp:        infexp
            exp ";" exp
            exp "::" ty
            "if" exp "then" exp "else" exp
            "case" exp {"|" exp "->" exp}
            "let" ["rec"] ["and"] dec {"and" dec} "in" exp
infexp:     appexp / infexp id infexp
appexp:     {atexp} atexp
atexp:      int / character / string
            id / cid
            "(" exp_seq ")"
            "[" exp_seq "]"
            "fn" {atexp} "->" exp

id:     [a-zA-Z0-9_']+ | [!%&$*+-/:<=>?@\~`^|]+
```

Notes
-----

- Unlike other MLs, `let ... and` is not simultaneous
- `::` cannot force two typevars to be different
  - e.g. `(:) :: (a, b list)` is the same as `(:) :: (a, a list)`
- There is no non-associative infix and it is not an error to
  mix left and right associative operators of the same level in
  the same expression


Questions
---------

Should `fun` and `nofun` be legal and polymorphic?

```
let fun = case (fn x -> x) | f -> f
let nofun x = case x | y -> y
```
