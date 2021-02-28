Synopsis
--------


Grammar
-------
```
top:        "datatype" "(" id,... ")" id "=" cdecls     Datatype decl w/ params
            "datatype" id "=" cdecls                    Datatype decl
            "infixl" / "infixr" int (id/cid)...         Infix decl
            "let" ["rec"] decls                         Top-level value decl

cdecls:     cdecls "|" cdecls                           Multiple decls
            cid [type]                                  Constructor decl

type:       btype ['->' type]                           Function type
btype:      atype id...                                 Constructor application
atype:      id                                          Named type
            "(" type,... ")"                            Tuple type

decls:      decls "and" decls                           Multiple declarations
            id clauses                                  Function decl
            aexp "=" exp                                Value decl
clauses:    clauses "|" clauses
            aexp... "=" exp

exp:        lexp ";" exp                                Sequence
            lexp "::" type                              Type restriction
            lexp
lexp:       "if" exp "then" exp "else" exp              Condition
            "case" exp ("|" exp "->" exp)...            Case analysis
            "let" ["rec"] <"and", decl> "in" exp        Local declarations
            iexp
iexp:       iexp id iexp                                Operator application
            {aexp} aexp                                 Function application
aexp:       int / character / string                    Literal
            id                                          Variable
            cid                                         Data constructor
            "(" exp,... ")"                             Tuple
            "[" exp,... "]"                             List
            "fn" clauses                                Lambda abstraction

id:         [a-zA-Z0-9_']+ | [!%&$*+-/:<=>?@~`^|]+
cid:        [A-Z][a-zA-Z0-9_']+
```

Notes
-----

- Unlike other MLs, `let ... and` is not simultaneous
- `::` cannot force two typevars to be different
  - e.g. `(:) :: (a, b list)` is the same as `(:) :: (a, a list)`
- There is no non-associative infix and it is not an error to
  mix left and right associative operators of the same level in
  the same expression

Functions
---------
- `* x y` multiply x and y
- `+ x y` add x to y
- `- x y` subtract y from x
- `/ x y` divide x by y
- `:= ref val` set ref to val
- `< x y`  x less than y
- `<= x y` x less than or equal to y
- `<> x y` x not equal to y
- `== x y` x equals y
- `> x y`  x greater than y
- `>= x y` x greater than or equal to y
- `chr n` return character n (mod 256)
- `chrstr c` return single-character string for character c
- `exit x` exit program with code x
- `implode cs` create string from list of characters
- `join ss` create string from list of strings
- `ord c` return character number
- `print x` print x to stdout
- `raise s` print an error message and exit program
- `read_file path` read string from file path or return NONE
- `rem x y` remainder of x divided by y
- `size s` length of string
- `sub s n` nth character of string
- `substr s i n` n-character substring starting at index i
- `write_file path str` write file path (result is success status)


Questions
---------

Should `fun` and `nofun` be legal and polymorphic?

```
let fun = case (fn x -> x) | f -> f
let nofun x = case x | y -> y
```
