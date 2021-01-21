Synopsis
--------


Grammar
-------
```
top:        ("datatype" datbind | infixdec | "let" ["rec"] <"and", dec> )
datbind:    (id | "(" id_seq ")") id "=" ty | conbind
conbind:    ["|"] cid [ty] {"|" cid [ty]}
infixdec:   "infixl"/"infixr" int {id/cid}
ty:         appty {'->' ty}
appty:      atty {id}
atty:       id / "(" ty_seq ")"
dec:        [id '::' ty] <"---", id {atexp} "=" exp>
            atexp "=" exp
exp:        lexp ";" exp
            lexp "::" ty
            lexp
lexp:       "if" exp "then" exp "else" exp
            "case" exp {"|" exp "->" exp}
            "let" ["rec"] <"and", dec> "in" exp
            infexp
infexp:     appexp / infexp id infexp
appexp:     {atexp} atexp
atexp:      int / character / string
            id ('@' atexp)
            cid
            "(" exp_seq ")"
            "[" exp_seq "]"
            "fn" ['::' ty] <"|", {atexp} "->" exp>

id:     [a-zA-Z0-9_']+ | [!%&$*+-/:<=>?@\~`^|]+
```

Notes
- `<M1, M2>` standards for optional `M1`, zero or more `M2` separated by `M1`
- `x_seq` is a comma-separated sequence of x with an optional trailing comma

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
