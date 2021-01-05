#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define expr(f,...) new(struct expr, .loc = loc, .form = f, __VA_ARGS__)

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct loc { char *fn; int ln; };

typedef struct value {
    enum { INT, CHAR, STRING, TUPLE, DATA, FN, PRIM } form;
    union {
        int     integer;
        unsigned character;
        char    *string;
        struct  { int n; struct value *tuple; };
        struct  { char *con; struct value *data; };
        struct  { struct expr *param, *body; struct environ *env; };
        struct value (*prim)(struct expr *c, struct value value);
    };
} value;

typedef struct environ {
    char    *id;
    value   value;
    struct environ *next;
} environ;

typedef struct type {
    enum { TYVAR, TY, FNTY, TUPLETY, ALIASTY } form;
    int         n;
    struct type **args;
    struct type *inst;
    char        *id;
} type;

typedef struct types {
    type    *type;
    struct types *next;
} types;

typedef struct typemap {
    type    *from;
    type    *to;
    struct typemap *next;
} typemap;

typedef struct typing {
    char    *id;
    type    *type;
    struct typing *next;
} typing;

typedef struct expr expr;
struct expr {
    enum { EINT, ECHAR, ESTRING, EVAR, ECON, ETUPLE, EFN,
        EAPP, ECASE, ELET, EREC, ETY
    } form;
    union {
        int     integer;
        unsigned character;
        char    *string;
        char    *id;
        struct { expr *lhs, *rhs; };
        struct { int n; expr **tuple; };
        struct { expr *lhs, *rhs, *body; } let;
        struct { struct dec *decs; expr *body; } rec;
        struct { expr *subject; struct rule *rules; };
        struct { expr *value; type *type; } typing;
    };
    struct loc loc;
};
struct dec { char *id; expr *fn; struct dec *next; };
struct rule { expr *lhs, *rhs; struct rule *next; };

struct op {
    char    *id;
    bool    iscon;
    int     lhs;
    int     rhs;
    struct op *next;
};

typedef enum {
    // Keep in order with `tokens[]`.
    // Keep relative order: CID, single-char punct, ID, reserved
    TEOF, TINT, TCHAR, TSTRING, TCID, TLP, TRP, TLB, TRB,
    TCOMMA, TSEMI, TID, TEQUAL, TLET, TREC, TAND, TIN, TFN,
    TARROW, TCASE, TBAR, TIF, TTHEN, TELSE, TINFIXL, TINFIXR,
    TDATATYPE, TTYPING,
} Token;

char *opchr = "!%&$*+-/:<=>?@\\~`^|";
char *tokens[] = {"eof", "int", "character", "string", "con id",
    "(", ")", "[", "]", ",", ";", "id", "=", "let", "rec",
    "and", "in", "fn", "->", "case", "|", "if", "then", "else",
    "infixl", "infixr", "datatype", "::", 0 };

char        source[128 * 1024];
char        *src;
struct loc  loc;
Token       token;
bool        peeked;
int         tokint;
char        tokbuf[sizeof source];
char        *tokstr;
char        *interns[65536];
int         ninterns;
char        *ignore;
int         coin_types;
struct op   *optab;
types       *nongeneric;
typing      *typetab;
typing      *contab;
type        *inttype, *chartype, *strtype;
value       trueval, falseval;
char        *consid, *nilid, *noneid, *someid, *trueid, *falseid, *andid, *orid;

void *pr(char *msg, ...);
type *ty(void);
type *ty_with_coining(void);
expr *expression(void);
expr *atexp(bool required);
type *check(expr *c, typing *env);
type *check_pattern(expr *c, typing **env);

char *intern(char *text) {
    for (int i = 0; i < ninterns; i++)
        if (!strcmp(interns[i], text))
            return interns[i];
    return interns[ninterns++] = strdup(text);
}

bool equal(value a, value b) {
    switch (a.form) {
    case INT:       return a.integer == b.integer;
    case CHAR:      return a.character == b.character;
    case STRING:    return !strcmp(a.string, b.string);

    case TUPLE:     for (int i = 0; i < a.n; i++)
                        if (!equal(a.tuple[i], b.tuple[i]))
                            return false;
                    return true;

    case DATA:      return  a.con == b.con &&
                            (!a.data || equal(*a.data, *b.data));

    case FN:        return  a.param == b.param &&
                            a.body == b.body &&
                            a.env == b.env;

    case PRIM:      return a.prim == b.prim;
    }
}

value integer(int i) {
    return (value) { INT, .integer = i };
}

value character(unsigned c) {
    return (value) { CHAR, .character = c };
}

value string(char *s) {
    return (value) { STRING, .string = s };
}

value tuple(int n, value *xs) {
    return (value) { TUPLE, .n = n, .tuple = xs };
}

value data(char *con, value *data) {
    return (value) { DATA, .con = con, .data = data };
}

value hd(value x) {
    return x.data->tuple[0];
}

value tl(value x) {
    return x.data->tuple[1];
}

bool consp(value x) {
    return x.con == consid;
}

value function(expr *param, expr *body, environ *env) {
    return (value) { FN, .param = param, .body = body, .env = env };
}

type *tycon(char *id, int n, type **args) {
    type **copy = memcpy(malloc(n * sizeof *copy), args, n * sizeof *copy);
    return new(type, TY, n, copy, .id = id);
}

type *tyvar(char *id) {
    return new(type, TYVAR, 0, 0, .id = id);
}

type *fntype(type *x, type *y) {
    return new(type, FNTY, 2, new(type*[2], x, y));
}

type *tupletype(int n, type **args) {
    return new(type, TUPLETY, n, .args = args);
}

bool isvar(type *type) {
    return type->form == TYVAR;
}

bool isfn(type *type) {
    return type->form == FNTY;
}

type *target(type *type) {
    if (isvar(type) && type->inst)
        return type->inst = target(type->inst);
    return type;
}

// Before printing, unnamed typevars need to be named.
// They need to be named in a consistent manner.
// Any types printed together, should pass the same `uid`.
// Multiple types printed from the same `pr()` call are consistent.

type *name_types(type *type, unsigned *uid) {
    type = target(type);
    if (type->form == TYVAR && type->id == 0)
        type->id = intern((char[]){ (*uid)++ + 'A', 0 });

    for (int i = 0; i < type->n; i++)
        name_types(type->args[i], uid);
    return type;
}

bool occurs_in(type *var, type *type) {
    type = target(type);
    if (isvar(type))
        return var == type;
    for (int i = 0; i < type->n; i++)
        if (occurs_in(var, type->args[i]))
            return true;
    return false;
}

// Make a fresh copy of a type respecting non-generic constraints.
// A fresh copy of a type is made every time a var, constructor,
// or type is mentioned in the source.

type *fresh(type *type, typemap **map) {
    typemap *local = 0;
    if (!map)
        map = &local;

    type = target(type);

    if (type->form == TYVAR) {
        // If this typevar occurs in any non-generic type, don't clone.
        for (types *i = nongeneric; i; i = i->next)
            if (occurs_in(type, i->type))
                return type;

        // Return a previous mapping if present.
        for (typemap *i = *map; i; i = i->next)
            if (i->from == type)
                return i->to;

        // Otherwise, make a new mapping.
        struct type *to = tyvar(0);
        *map = new(typemap, type, to, *map);
        return to;
    }

    else if (type->n == 0)
        return type; // No clone needed if there are no type args.

    else {

        // Clone all the arguments of a concrete or alias type.

        struct type *inst = type->inst? fresh(type->inst, map): 0;
        struct type **args = malloc(type->n * sizeof *args);

        for (int i = 0; i < type->n; i++)
            args[i] = fresh(type->args[i], map);

        return new(struct type, type->form, type->n, args,
                   .id = type->id,
                   .inst = inst);
    }
}

// Return true if the two types are compatible.
// Typevars involved are updated destructively.
// Because of that, if they do not match, the checker should exit.
// If an alias is given, its instantiation is used instead.
// See `appty()` for more info about how instantiation works.

bool unifies(type *a, type *b) {
    a = target(a);
    b = target(b);

    if (isvar(a))
        if (occurs_in(a, b))
            return a == b;
        else
            return a->inst = b, true;

    if (isvar(b))
        return unifies(b, a);

    if (a->form == ALIASTY)
        a = a->inst;
    if (b->form == ALIASTY)
        b = b->inst;

    if (a->form != b->form || a->id != b->id || a->n != b->n)
        return false;

    for (int i = 0; i < a->n; i++)
        if (!unifies(a->args[i], b->args[i]))
            return false;
    return true;
}

void *print_type(type *t) {
    switch (t->form) {
    case TYVAR:     return pr("%s", t->id);

    case ALIASTY:
    case TY:        if (t->n == 0)
                        return pr("%s", t->id);

                    else if (t->n == 1 && isfn(t->args[0]))
                        return pr("(%t) %s", t->args[0], t->id);

                    else if (t->n == 1)
                        return pr("%t %s", t->args[0], t->id);

                    else if (t->n > 1) {
                        for (int i = 0; i < t->n; i++)
                            pr("%s%t", i? ", ": "(", t->args[i]);
                        return pr(")");
                    }

    case FNTY:      return pr(isfn(t->args[0])? "(%t) -> %t": "%t -> %t",
                        t->args[0], t->args[1]);

    case TUPLETY:   pr("(");
                    for (int i = 0; i < t->n; i++)
                        pr("%s%t", i? ", ": "", t->args[i]);
                    return pr(")");
    }
}

void *print_expression(expr *e) {
    switch (e->form) {
    case EINT:      return pr("%d", e->integer);
    case ECHAR:     return pr("'%s'", e->character);
    case ESTRING:   return pr("\"%s\"", e->string);
    case EVAR:      return pr("%s", e->id);
    case ECON:      return pr("%s", e->id);

    case ETUPLE:    pr("(");
                    for (int i = 0; i < e->n; i++)
                        pr("%s%e", i? ", ": "", e->tuple[i]);
                    return pr(")");

    case EFN:       return pr("fn %e -> %e", e->lhs, e->rhs);

    case EAPP:      return pr("(%e %e)", e->lhs, e->rhs);

    case ECASE:     pr("(case %e", e->subject);
                    for (struct rule *i = e->rules; i; i = i->next)
                        pr(" | %e -> %e", i->lhs, i->rhs);
                    return pr(")");

    case ELET:      return pr("(let %e = %e in %e)",
                              e->let.lhs, e->let.rhs, e->let.body);

    case EREC:      pr("(let rec");
                    for (struct dec *i = e->rec.decs; i; i = i->next)
                        pr(" %s = %e%s", i->id, i->fn, i->next? " and": "");
                    return pr(" in %e)", e->rec.body);

    case ETY:       return pr("((%e) :: %t)", e->typing.value, e->typing.type);
    }
}

void *print_value(value x) {
    switch (x.form) {
    case INT:       return pr("%d", x.integer);
    case CHAR:      return pr("%c", x.character);
    case STRING:    return pr("%s", x.string);

    case TUPLE:     pr("(");
                    for (int i = 0; i < x.n; i++)
                        pr("%s%x", i? ", ": "", x.tuple[i]);
                    return pr(")");

    case DATA:      if (x.con == nilid)
                        return pr("[]");
                    else if (consp(x)) { // Print lists specially.
                        pr("[");
                        for (value i = x; consp(i); i = tl(i))
                            pr("%x%s", hd(i), consp(tl(i))? ", ": "");
                        return pr("]");
                    }
                    else if (x.data)
                        return pr("%s %x", x.con, *x.data);
                    else
                        return pr("%s", x.con);

    case FN:        return pr("(#fn %s:%d)", x.param->loc.fn, x.param->loc.ln);

    case PRIM:      return pr("(#prim)");
    }
}

void *vpr(char *msg, va_list ap) {
    unsigned uid = 0;
    char *msg2;

    for (char *s = msg; *s; s++)
        if (*s != '%')
            putchar(*s);
        else switch (*++s) {
        case 'c': printf("%c", va_arg(ap, unsigned)); break;
        case 'd': printf("%d", va_arg(ap, int)); break;
        case 's': printf("%s", va_arg(ap, char*)); break;
        case 'e': print_expression(va_arg(ap, expr*)); break;
        case 't': print_type(name_types(va_arg(ap, type*), &uid)); break;
        case 'x': print_value(va_arg(ap, value)); break;
        case '*':
            msg2 = va_arg(ap, char*);
            vpr(msg2, *va_arg(ap, va_list*));
            break;
        }
}

void *pr(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    vpr(msg, ap);
    va_end(ap);
}

_Noreturn void *syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("(boot) error %s:%d: %*.\n", loc.fn, loc.ln, msg, &ap);
    exit(1);
}

void open_source(char *filename) {
    loc = (struct loc){ strdup(filename), 1 };
    src = source, peeked = false;
    FILE *file = fopen(filename, "rb");
    if (!file)
        syntax("cannot open");
    source[fread(source, 1, sizeof source, file)] = 0;
    fclose(file);
}

int chr(void) {
    char c = *src++;

    if (c == '\\')
        switch ((c = *src++)) {
        case 'n':   return '\n'; break;
        case 't':   return '\t'; break;
        case '0':   return '\0'; break;
        case 0:     syntax("eof in escape"); break;
        default:    return c;
        }

    return c;
}


// Get the next token.
// The type is set in `token`.
// `toks`, `toki` contain the values of a token depending on the type.

Token next(void) {
    char *t = tokbuf;

    if (peeked)
        return peeked = false, token;

    for ( ; isspace(*src) || *src == '#'; src++)
        if (*src == '\n')
            loc.ln++;
        else if (*src == '#')
            while (src[1] && src[1] != '\n')
                src++;

    if (!*src)
        return token = TEOF;

    if (isdigit(src[*src == '-' || *src == '+'])) {
        tokint = strtol(src, &src, 10);
        return token = TINT;
    }

    for (Token i = TCID + 1; i < TID; i++)
        if (src[0] == tokens[i][0]) {
            src++;
            return token = i;
        }

    if (*src == '"') {
        char quote = *src++;
        while (true)
            if (*src == '\n' || !*src)
                syntax("unclosed string");
            else if (*src == quote) {
                src++;
                *t = 0, tokstr = intern(tokbuf);
                return token = TSTRING;
            }
            else
                *t++ = chr();
    }

    if (*src == '\'') {
        src++;
        tokint = chr();
        if (*src != '\'')
            syntax("unclosed character");
        src++;
        return token = TCHAR;
    }

    if (isalnum(*src) || *src == '_' || *src == '\'')
        while (isalnum(*src) || *src == '_' || *src == '\'')
            *t++ = *src++;
    else if (*src && strchr(opchr, *src))
        while (*src && strchr(opchr, *src))
            *t++ = *src++;
    else
        syntax("bad token (%c)", *src);

    *t = 0, tokstr = intern(tokbuf);
    for (Token i = TID + 1; tokens[i]; i++)
        if (!strcmp(tokbuf, tokens[i]))
            return token = i;

    return token = (isupper(*tokstr) || tokstr == consid)? TCID: TID;
}

bool want(Token t) {
    peeked = (t != next());
    return !peeked;
}

bool peek(Token t) {
    peeked = next();
    return t == token;
}

void need(Token t) {
    if (!want(t))
        syntax("needed %s not %s", tokens[t], tokens[token]);
}

// Return the descriptor for the infix operator token up next.
struct op *op_if_next(int level) {
    if (peek(TID) || peek(TCID))
        for (struct op *i = optab; i; i = i->next)
            if (i->id == tokstr && (level < 0 || level == i->lhs))
                return i;
    return 0;
}

expr *conref(char *id) {
    return expr(ECON, .id = id);
}

expr *binapp(bool iscon, char *id, expr *lhs, expr *rhs) {
    expr *f = expr(iscon? ECON: EVAR, .id = id);
    expr *x = expr(ETUPLE, .n = 2, .tuple = new(expr*[2], lhs, rhs));
    return expr(EAPP, .lhs = f, .rhs = x);
}

expr *ifmatch(expr *test, expr *affirm, expr *negative) {
    struct rule *rules =
        new(struct rule, conref(falseid), negative,
        new(struct rule, conref(trueid), affirm, 0));
    return expr(ECASE, .subject = test, .rules = rules);
}

expr *listexp(void) {
    if (want(TRB))
        return conref(nilid);

    expr *hd = expression();

    if (want(TCOMMA)) {
        expr *tl = listexp();
        return binapp(true, consid, hd, tl);
    } else {
        need(TRB);
        return binapp(true, consid, hd, conref(nilid));
    }
}

expr *funexp(Token delim) {
    if (want(delim))
        return expression();
    expr *lhs = atexp(true);
    expr *rhs = funexp(delim);
    return expr(EFN, .lhs = lhs, .rhs = rhs);
}

expr *atexp(bool required) {

    // Do not consume operator as argument.
    if (!required && op_if_next(-1))
        return 0;

    if (want(TINT))     return expr(EINT, .integer = tokint);
    if (want(TCHAR))    return expr(ECHAR, .character = tokint);
    if (want(TSTRING))  return expr(ESTRING, .string = tokstr);
    if (want(TID))      return expr(EVAR, .id = tokstr);
    if (want(TCID))     return expr(ECON, .id = tokstr);
    if (want(TLB))      return listexp();
    if (want(TFN))      return funexp(TARROW);

    if (want(TLP)) { // Tuple.
        int     n = 0;
        expr    **tuple = 0;

        do {
            if (peek(TRP)) break;
            tuple = realloc(tuple, (n + 1) * sizeof *tuple);
            tuple[n++] = expression();
        } while (want(TCOMMA));
        need(TRP);

        return n == 1? tuple[0]:
               expr(ETUPLE, .n = n, .tuple = tuple);
    }

    return required? syntax("need expression before %s", tokens[token]): 0;
}

expr *appexp(void) {
    expr *lhs = atexp(true);
    expr *rhs;
    while ((rhs = atexp(false)))
        lhs = expr(EAPP, .lhs = lhs, .rhs = rhs);
    return lhs;
}

expr *infexp(int level) {
    if (level == 11)
        return appexp();

    else {
        expr *lhs = infexp(level + 1);

        struct op *op;
        while ((op = op_if_next(level))) {
            next();

            expr *rhs = infexp(op->rhs);

            // Hijack `&&` and `||` and turn into conditionals.
            lhs = op->id == andid?  ifmatch(lhs, rhs, conref(falseid)):
                  op->id == orid?   ifmatch(lhs, conref(trueid), rhs):
                                    binapp(op->iscon, op->id, lhs, rhs);
        }
        return lhs;
    }
}

struct dec *rec_decs(void) {
    struct dec *decs = 0;
    struct dec **indir = &decs;

    want(TAND);
    do {
        char    *id = (need(TID), tokstr);
        expr    *body = funexp(TEQUAL);

        *indir = new(struct dec, id, body, 0);
        indir = &(*indir)->next;

    } while (want(TAND));

    return decs;
}

expr **nonrec_decs(expr **indir) {
    want(TAND);
    do {
        expr    *lhs = atexp(true);
        expr    *rhs = funexp(TEQUAL);

        *indir = expr(ELET, .let = { lhs, rhs, 0 });
        indir = &(*indir)->let.body;

    } while (want(TAND));

    return indir;
}

expr *complex(void) {
    if (want(TLET))
        if (want(TREC)) {
            struct dec *decs = rec_decs();
            need(TIN);
            expr *body = expression();
            return expr(EREC, .rec = { decs, body });
        }
        else {
            expr *decs = 0;
            expr **indir = nonrec_decs(&decs);
            need(TIN);
            *indir = expression();
            return decs;
        }

    if (want(TCASE)) {
        expr *subject = expression();

        struct rule *rules = 0;
        struct rule **indir = &rules;

        while (want(TBAR)) {
            expr    *lhs = expression();
            expr    *rhs = (need(TARROW), expression());

            *indir = new(struct rule, lhs, rhs, 0);
            indir = &(*indir)->next;
        }

        return expr(ECASE, .subject = subject, .rules = rules);
    }

    if (want(TIF)) {
        expr *test = expression();
        expr *affirm = (need(TTHEN), expression());
        expr *negative = (need(TELSE), expression());
        return ifmatch(test, affirm, negative);
    }

    return infexp(0);
}

expr *expression(void) {
    expr *lhs = complex();

    if (want(TSEMI)) {
        expr    *body = expression();
        expr    *_ = expr(EVAR, .id = ignore);
        return expr(ELET, .let = { _, lhs, body });
    }

    else if (want(TTYPING)) {
        type *type = ty_with_coining();
        return expr(ETY, .typing = { lhs, type });
    }

    else
        return lhs;
}

void infixdec(int adjust) {
    int lhs = (need(TINT), tokint);
    int rhs = lhs + adjust;
    while (want(TID) || want(TCID))
        optab = new(struct op, tokstr, token == TCID, lhs, rhs, optab);
}

type *find(char *id, typing *env) {
    for (typing *i = env; i; i = i->next)
        if (i->id == id)
            return target(i->type);
    return 0;
}

type *atty(void) {
    if (want(TLP)) { // Tuple.
        int     n = 0;
        type    **tuple = 0;

        do {
            if (peek(TRP)) break;
            tuple = realloc(tuple, (n + 1) * sizeof *tuple);
            tuple[n++] = ty();
        } while (want(TCOMMA));

        need(TRP);
        return n == 1? *tuple: tupletype(n, tuple);
    }

    // Named type (typevar, regular, or alias).
    else {
        char *id = (need(TID), tokstr);
        type *type = find(id, typetab);

        if (!type && coin_types) { // Coin new type if enabled.
            type = tyvar(id);
            typetab = new(typing, id, type, typetab);
            nongeneric = new(types, type, nongeneric);
            return type;
        }
        else {
            if (!type)
                syntax("undefined type: %s", id);
            if (type->n)
                syntax("type requires args: %t", type);
            return type;
        }
    }
}

type *appty(void) {
    type *arg = atty();

    while (want(TID)) {
        char    *id = tokstr;
        type    *type = find(id, typetab);

        if (!type)
            syntax("undefined type: %s", id);

        if (type->form != TY && type->form != ALIASTY)
            syntax("not a type constructor: %t", type);

        if (type->n == 0)
            syntax("type constructor takes no args: %t", type);

        // Only try to decompose arg tuple if constructor has arity > 1.
        if (type->n > 1 && type->n != (arg->form == TUPLETY? arg->n: 1))
            syntax("wrong type constructor arity: %t vs %t", type, arg);

        struct type **args = malloc(type->n * sizeof *args);
        memcpy(args,
               type->n == 1? &arg: arg->args,
               type->n * sizeof *args);

        if (type->form == ALIASTY) {
            // Instantiate the alias.
            // Copy the target and associate alias typevars with given args.
            // The only typevars in the target type must have come from the
            // alias. So binding the target args is reflected in the target.
            // See `unifies()` for more information.

            type = fresh(type, 0);

            for (int i = 0; i < type->n; i++)
                unifies(type->args[i], args[i]);

            arg = new(struct type, ALIASTY, type->n, args,
                      .id = type->id,
                      .inst = type->inst);
        }
        else if (type->form == TY)
            arg = tycon(type->id, type->n, args);
    }
    return arg;
}

type *ty(void) {
    type *lhs = appty();
    return want(TARROW)? fntype(lhs, ty()): lhs;
}

// Read a type and treat undefined types as type variables.
type *ty_with_coining(void) {
    typing     *old = typetab;
    types       *oldng = nongeneric;

    coin_types++;
    type *type = ty();
    coin_types--;

    typetab = old;
    nongeneric = oldng;
    return type;
}

void conbind(type *datatype) {
    want(TBAR);

    do {
        char    *id = (need(TCID), tokstr);
        type    *arg = (peek(TID) || peek(TLP))? ty(): 0;
        type    *type = arg? fntype(arg, datatype): datatype;

        struct type *existing = find(id, contab);
        if (existing)
            syntax("constructor already defined: %s :: %t", id, existing);

        contab = new(typing, id, type, contab);

    } while (want(TBAR));
}

void datbind(void) {
    char    *type_id = 0;
    type    **args = 0;
    int     nargs = 0;

    if (want(TLP)) { // Parenthesized type arguments
        do {
            if (peek(TRP)) break;
            args = realloc(args, (nargs + 1) * sizeof *args);
            args[nargs++] = (need(TID), tyvar(tokstr));
        } while (want(TCOMMA));
        need(TRP);
        type_id = (need(TID), tokstr);
    }
    else if (want(TID)) { // Single or no type arguments.
        type_id = tokstr;
        if (want(TID)) {
            args = new(struct type*[1], tyvar(type_id));
            nargs = 1;
            type_id = tokstr;
        }
    }

    if (find(type_id, typetab))
        syntax("type already defined: %s", type_id);

    need(TEQUAL);

    if (peek(TCID)) { // Define datatype.
        type *datatype = tycon(type_id, nargs, args);
        typetab = new(typing, type_id, datatype, typetab);

        for (int i = 0; i < nargs; i++)
            typetab = new(typing, args[i]->id, args[i], typetab);

        conbind(datatype);

        for (int i = 0; i < nargs; i++)
            typetab = typetab->next;
    }

    else {
        // Define type alias.
        // See `appty()` and `unifies()` for more info on aliases.

        for (int i = 0; i < nargs; i++)
            typetab = new(typing, args[i]->id, args[i], typetab);

        type *inst = ty();
        type *alias = new(type, ALIASTY, nargs, args,
                          .id = type_id,
                          .inst = inst);

        for (int i = 0; i < nargs; i++)
            typetab = typetab->next;

        typetab = new(typing, type_id, alias, typetab);
    }
}

expr **top(expr **nextp) {
    while (!peek(TEOF))
        if (want(TINFIXL)) infixdec(1);
        else if (want(TINFIXR)) infixdec(0);
        else if (want(TDATATYPE)) datbind();
        else {
            need(TLET);
            if (want(TREC)) {
                struct dec *decs = rec_decs();

                *nextp = expr(EREC, .rec = { decs, 0 });
                nextp = &(*nextp)->rec.body;
            }
            else
                nextp = nonrec_decs(nextp);
        }

    if (*nextp == 0)
        *nextp = expr(ETUPLE);
    return nextp;
}

_Noreturn void *semantic(expr *c, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("(boot) error %s:%d: %*.\n", c->loc.fn, c->loc.ln, msg, &ap);
    exit(1);
}

type *unify(expr *c, type *a, type *b) {
    if (!unifies(a, b))
        semantic(c, "type error:\n %t\n %t", a, b);

    a = target(a);
    b = target(b);
    return a->form == ALIASTY? a:
           b->form == ALIASTY? b:
           a;
}

/*
    Type Inferencing.
    - Based on Luca Cardelli's "Basic Polymorphic Typechecking"
    - Manage nongeneric list (typevars that cannot be generic)
      - push type for anything not defined on the l.h.s. of let
      - make sure to pop at appropriate time
      - popping does not neccessarily mean a type becomes
        generic, there can be multiple entries
    - Create fresh types when a var, constructor, or type is
      visibly referenced in an expression
*/
type *check(expr *c, typing *env) {
    type    *type, *lhs, *rhs, **args;
    types   *oldng = nongeneric;

    switch (c->form) {
    case EINT:      return inttype;
    case ECHAR:     return chartype;
    case ESTRING:   return strtype;

    case EVAR:      type = find(c->id, env);
                    if (!type)
                        semantic(c, "undefined: %s", c->id);
                    return fresh(type, 0);

    case ECON:      type = find(c->id, contab);
                    if (!type)
                        semantic(c, "undefined constructor: %s", c->id);
                    return fresh(type, 0);

    case ETUPLE:    args = malloc(c->n * sizeof *args);
                    for (int i = 0; i < c->n; i++)
                        args[i] = check(c->tuple[i], env);
                    return tupletype(c->n, args);

    case EFN:       lhs = check_pattern(c->lhs, &env);
                    rhs = check(c->rhs, env);
                    nongeneric = oldng;
                    return fntype(lhs, rhs);

    case EAPP:      lhs = check(c->lhs, env);
                    rhs = check(c->rhs, env);

                    lhs = target(lhs);

                    if (isfn(lhs)) { // Cleaner error with only arg.
                        unify(c, lhs->args[0], rhs);
                        return lhs->args[1];
                    }
                    else {
                        type = tyvar(0);
                        unify(c, lhs, fntype(rhs, type));
                        return target(type);
                    }

    case ECASE:     lhs = check(c->subject, env);
                    rhs = tyvar(0);

                    for (struct rule *i = c->rules; i; i = i->next) {
                        typing *local = env;
                        unify(i->lhs, lhs, check_pattern(i->lhs, &local));
                        unify(i->rhs, rhs, check(i->rhs, local));

                        // The symbols in the pattern can be released now.
                        // Any escapee is either rightfully polymorphic
                        // or has another entry in the non-generic list.
                        nongeneric = oldng;
                    }

                    return rhs;

    case ELET:      rhs = check(c->let.rhs, env);
                    lhs = check_pattern(c->let.lhs, &env);
                    nongeneric = oldng;     // All let symbols are polymorphic.
                    unify(c->let.lhs, lhs, rhs);
                    type = check(c->let.body, env);
                    return type;

    case EREC:      // Pre-define all functions with non-generic typevars.
                    // All functions remain non-generic over all definitions.
                    for (struct dec *i = c->rec.decs; i; i = i->next) {
                        struct type *tmp = tyvar(0);

                        if (i->fn->form != EFN)
                            semantic(i->fn, "let rec only defines functions");

                        env = new(typing, i->id, tmp, env);
                        nongeneric = new(types, tmp, nongeneric);
                    }

                    // Resolve typevars with r.h.s.
                    for (struct dec *i = c->rec.decs; i; i = i->next) {
                        lhs = find(i->id, env);
                        rhs = check(i->fn, env);
                        unify(i->fn, lhs, rhs);
                    }

                    nongeneric = oldng;
                    return check(c->rec.body, env);

    case ETY:       lhs = check(c->typing.value, env);
                    rhs = fresh(c->typing.type, 0); // (don't affect target)
                    return unify(c, rhs, lhs);
    }
}

type *check_pattern(expr *c, typing **env) {
    type    *type, *lhs, *rhs, **args;

    switch (c->form) {
    case EINT:      return inttype;
    case ECHAR:     return chartype;
    case ESTRING:   return strtype;

    case EVAR:      type = tyvar(0);
                    nongeneric = new(types, type, nongeneric);
                    if (c->id != ignore)
                        *env = new(typing, c->id, type, *env);
                    return type;

    case ECON:      type = find(c->id, contab);
                    if (!type)
                        semantic(c, "undefined constructor: %s", c->id);
                    return fresh(type, 0);

    case ETUPLE:    args = malloc(c->n * sizeof *args);
                    for (int i = 0; i < c->n; i++)
                        args[i] = check_pattern(c->tuple[i], env);
                    return tupletype(c->n, args);

    case EAPP:      if (c->lhs->form != ECON)
                        semantic(c->lhs, "invalid constructor pattern");

                    lhs = check_pattern(c->lhs, env);
                    rhs = check_pattern(c->rhs, env);

                    lhs = target(lhs);

                    if (isfn(lhs)) { // Cleaner error with only arg.
                        unify(c, lhs->args[0], rhs);
                        return lhs->args[1];
                    }
                    else {
                        type = tyvar(0);
                        unify(c, lhs, fntype(rhs, type));
                        return target(type);
                    }

    case ETY:       type = check_pattern(c->typing.value, env);
                    return unify(c, c->typing.type, type);

    default:        return semantic(c, "invalid pattern");
    }
}

bool bind_pattern(expr *pat, value x, environ **env) {
    switch (pat->form) {
    case EINT:      return pat->integer == x.integer;
    case ESTRING:   return !strcmp(pat->string, x.string);

    case EVAR:      if (pat->id != ignore)
                        *env = new(environ, pat->id, x, *env);
                    return true;

    case ECON:      return pat->id == x.con;

    case ETUPLE:    for (int i = 0; i < pat->n; i++)
                        if (!bind_pattern(pat->tuple[i], x.tuple[i], env))
                            return false;
                    return true;

    case EAPP:      if (pat->lhs->form != ECON)
                        semantic(pat, "INTERNAL ERROR: DECON");

                    return  pat->lhs->id == x.con
                            && bind_pattern(pat->rhs, *x.data, env);

    default:        semantic(pat, "INTERNAL ERROR: PATTERN (%e)", pat);
    }
}

value eval(expr *code, environ *env) {
    value   *values;
    value   x, y;
    environ *saved;

tail:
    switch (code->form) {
    case EINT:      return integer(code->integer);
    case ECHAR:     return character(code->character);
    case ESTRING:   return string(code->string);

    case EVAR:      for (environ *i = env; i; i = i->next)
                        if (i->id == code->id)
                            return i->value;

                    semantic(code, "INTERNAL ERROR: VARIABLE (%s)", code->id);

    case ECON:      return data(code->id, 0);

    case ETUPLE:    if (code->n == 0)
                        return tuple(0, 0);

                    values = malloc(code->n * sizeof *values);
                    for (int i = 0; i < code->n; i++)
                        values[i] = eval(code->tuple[i], env);
                    return tuple(code->n, values);

    case EFN:       return function(code->lhs, code->rhs, env);

    case EAPP:      x = eval(code->lhs, env);
                    y = eval(code->rhs, env);

                    if (x.form == PRIM)
                        return x.prim(code, y);

                    if (x.form == DATA)
                        return data(x.con, new(value[1], y));

                    if (x.form == FN) {
                        env = x.env;
                        if (bind_pattern(x.param, y, &env)) {
                            code = x.body;
                            goto tail;
                        } else
                            semantic(code,
                                     "REFUTED ARG:\n%s:%d: %e\n%x",
                                     x.param->loc.fn,
                                     x.param->loc.ln,
                                     x.param,
                                     y);
                    }

                    semantic(code, "INTERNAL ERROR: CALL (%x)", x);

    case ECASE:     x = eval(code->subject, env);

                    saved = env;
                    for (struct rule *i = code->rules; i; i = i->next)
                        if (bind_pattern(i->lhs, x, &env)) {
                            code = i->rhs;
                            goto tail;
                        } else
                            env = saved;

                    semantic(code->subject, "NO_MATCH: %x", x);

    case ELET:      x = eval(code->let.rhs, env);

                    if (bind_pattern(code->let.lhs, x, &env)) {
                        code = code->let.body;
                        goto tail;
                    } else
                        semantic(code,
                                 "REFUTED VALUE:\n%e\n%x",
                                 code->let.lhs,
                                 x);

    case EREC:      saved = env;
                    for (struct dec *i = code->rec.decs; i; i = i->next)
                        env = new(environ,
                                  i->id,
                                  function(i->fn->lhs, i->fn->rhs, 0),
                                  env);

                    for (environ *i = env; i != saved; i = i->next)
                        i->value.env = env;

                    code = code->rec.body;
                    goto tail;

    case ETY:       code = code->typing.value;
                    goto tail;
    }
}

value prim_raise(expr *code, value x) {
    semantic(code, x.string);
}
value prim_exit(expr *code, value x) {
    exit(x.integer);
}
value prim_read_file(expr *code, value x) {
    FILE *file = fopen(x.string, "rb");

    if (!file)
        return data(noneid, 0);

    fseek(file, 0, SEEK_END);

    size_t  size = ftell(file);
    char    *text = malloc(size + 1);

    rewind(file);
    fread(text, 1, size, file);
    text[size] = 0;

    fclose(file);
    return data(someid, new(value[1], string(text)));
}
value prim_write_file(expr *code, value x) {
    FILE *file = fopen(x.tuple[0].string, "wb");

    if (!file)
        return falseval;

    fprintf(file, "%s", x.tuple[1].string);
    fclose(file);
    return trueval;
}
value prim_print(expr *code, value x) {
    pr("%x", x);
    return x;
}
value prim_ord(expr *code, value x) {
    return integer(x.character);
}
value prim_chr(expr *code, value x) {
    return character(x.integer);
}
value prim_set(expr *code, value x) {
    return (*x.tuple[0].data = x.tuple[1]);
}
value prim_add(expr *code, value x) {
    return integer(x.tuple[0].integer + x.tuple[1].integer);
}
value prim_subtract(expr *code, value x) {
    return integer(x.tuple[0].integer - x.tuple[1].integer);
}
value prim_multiply(expr *code, value x) {
    return integer(x.tuple[0].integer * x.tuple[1].integer);
}
value prim_divide(expr *code, value x) {
    if (x.tuple[1].integer == 0)
        semantic(code, "DIVIDE_BY_ZERO");
    return integer(x.tuple[0].integer / x.tuple[1].integer);
}
value prim_remainder(expr *code, value x) {
    if (x.tuple[1].integer == 0)
        semantic(code, "DIVIDE_BY_ZERO");
    return integer(x.tuple[0].integer % x.tuple[1].integer);
}
value prim_less(expr *code, value x) {
    return x.tuple[0].integer < x.tuple[1].integer? trueval: falseval;
}
value prim_less_equal(expr *code, value x) {
    return x.tuple[0].integer <= x.tuple[1].integer? trueval: falseval;
}
value prim_greater(expr *code, value x) {
    return x.tuple[0].integer > x.tuple[1].integer? trueval: falseval;
}
value prim_greater_equal(expr *code, value x) {
    return x.tuple[0].integer >= x.tuple[1].integer? trueval: falseval;
}
value prim_equal(expr *code, value x) {
    return equal(x.tuple[0], x.tuple[1])? trueval: falseval;
}
value prim_not_equal(expr *code, value x) {
    return !equal(x.tuple[0], x.tuple[1])? trueval: falseval;
}
value prim_size(expr *code, value x) {
    return integer(strlen(x.string));
}
value prim_sub(expr *code, value x) {
    int i = x.tuple[1].integer;
    if (i < 0)
        semantic(code, "INDEX %d", i);
    return character(x.tuple[0].string[i]);
}
value prim_substring(expr *code, value x) {
    int i = x.tuple[1].integer;
    int count = x.tuple[2].integer;
    if (i < 0)
        semantic(code, "INDEX %d", i);
    if (count < 0)
        semantic(code, "COUNT %d", count);

    char *text = malloc(count + 1);
    memcpy(text, x.tuple[0].string + i, count);
    text[count] = 0;

    return string(text);
}
value prim_implode(expr *code, value x) {
    int n = 0;
    for (value i = x; consp(i); i = tl(i))
        n++;
    char *text = malloc(n + 1);

    n = 0;
    for (value i = x; consp(i); i = tl(i))
        text[n++] = hd(i).character;
    text[n] = 0;

    return string(text);
}
value prim_concat(expr *code, value x) {
    int n = 0;
    for (value i = x; consp(i); i = tl(i))
        n += strlen(hd(i).string);

    char *text = malloc(n + 1);
    char *p = text;

    for (value i = x; consp(i); i = tl(i))
        for (char *s = hd(i).string; *s; )
            *p++ = *s++;
    *p = 0;
    return string(text);
}

void basis(char *id,
            type *arg,
            type *ret,
            value (*prim)(expr *code, value value),
            typing **cenv,
            environ **renv)
{
    id = intern(id);
    type *type = fntype(arg, ret);
    value value = (struct value) { PRIM, .prim = prim };
    *cenv = new(typing, id, type, *cenv);
    *renv = new(environ, id, value, *renv);
}

int main(int argc, char **argv) {
    typing      *cenv = 0;
    environ     *renv = 0;

    // Set up values the compiler needs.
    consid      = intern(":");
    nilid       = intern("NIL");
    noneid      = intern("NONE");
    someid      = intern("SOME");
    trueid      = intern("True");
    falseid     = intern("False");
    andid       = intern("&&");
    orid        = intern("||");
    ignore      = intern("_");
    falseval    = data(intern("False"), 0);
    trueval     = data(intern("True"), 0);
    inttype     = tycon(intern("int"), 0, 0);
    chartype    = tycon(intern("char"), 0, 0);
    strtype     = tycon(intern("string"), 0, 0);
    typetab     = new(typing, inttype->id, inttype, typetab);
    typetab     = new(typing, chartype->id, chartype, typetab);
    typetab     = new(typing, strtype->id, strtype, typetab);

    // Set up primitive functions.
    type    *any = tyvar(0);
    type    *booltype = tycon(intern("bool"), 0, 0);
    type    *reftype = tycon(intern("ref"), 1, new(type*[1], any));
    type    *str2 = tupletype(2, new(type*[2], strtype, strtype));
    type    *int2 = tupletype(2, new(type*[2], inttype, inttype));
    type    *any2 = tupletype(2, new(type*[2], any, any));
    type    *refany = tupletype(2, new(type*[2], reftype, any));
    type    *strlist = tycon(intern("list"), 1, new(type*[1], strtype));
    type    *charlist = tycon(intern("list"), 1, new(type*[1], chartype));
    type    *strint = tupletype(2, new(type*[2], strtype, inttype));
    type    *strint2 = tupletype(3, new(type*[3], strtype, inttype, inttype));
    type    *stropt = tycon(intern("option"), 1, new(type*[1], strtype));

    basis("raise", strtype, any, prim_raise, &cenv, &renv);
    basis("exit", inttype, any, prim_exit, &cenv, &renv);
    basis("read_file", strtype, stropt, prim_read_file, &cenv, &renv);
    basis("write_file", str2, stropt, prim_write_file, &cenv, &renv);
    basis("print", any, any, prim_print, &cenv, &renv);
    basis(":=", refany, any, prim_set, &cenv, &renv);
    basis("ord", chartype, inttype, prim_ord, &cenv, &renv);
    basis("chr", inttype, chartype, prim_chr, &cenv, &renv);
    basis("sub", strint, chartype, prim_sub, &cenv, &renv);
    basis("substring", strint2, strtype, prim_substring, &cenv, &renv);
    basis("size", strtype, inttype, prim_size, &cenv, &renv);
    basis("implode", charlist, strtype, prim_implode, &cenv, &renv);
    basis("concat", strlist, strtype, prim_concat, &cenv, &renv);
    basis("+", int2, inttype, prim_add, &cenv, &renv);
    basis("-", int2, inttype, prim_subtract, &cenv, &renv);
    basis("*", int2, inttype, prim_multiply, &cenv, &renv);
    basis("/", int2, inttype, prim_divide, &cenv, &renv);
    basis("rem", int2, inttype, prim_remainder, &cenv, &renv);
    basis("<", int2, booltype, prim_less, &cenv, &renv);
    basis("<=", int2, booltype, prim_less_equal, &cenv, &renv);
    basis(">", int2, booltype, prim_greater, &cenv, &renv);
    basis(">=", int2, booltype, prim_greater_equal, &cenv, &renv);
    basis("<>", any2, booltype, prim_not_equal, &cenv, &renv);
    basis("==", any2, booltype, prim_equal, &cenv, &renv);

    expr    *code = 0;
    expr    **cont = &code;

    value arg = data(nilid, 0);
    for (int i = argc; i-- > 1; ) {
        value hd = string(argv[i]);
        value q = tuple(2, new(value[2], hd, arg));
        arg = data(consid, new(value[1], q));
    }
    cenv = new(typing, intern("argv"), strlist, cenv);
    renv = new(environ, intern("argv"), arg, renv);


    open_source("boot.ml");
    cont = top(cont);

    if (argv[1]) {
        open_source(argv[1]);
        cont = top(cont);
    }

    check(code, cenv);
    eval(code, renv);

    need(TEOF);
    puts("done.");
}
