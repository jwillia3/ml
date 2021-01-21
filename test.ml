# let x = 1
# and y = 3
# in 2

# let rec f x = 1 in 2
datatype bool = TRUE | FALSE
datatype a tree = EMPTY int | NODE (a tree, a, a tree)
datatype (a,b) map = EMPTY_MAP | MAP(a,b, (a,b) map)

infixl 10 + * /

let f :: int = 1
and y = 20

let z = 300
