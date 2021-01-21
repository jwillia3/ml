#define new(t,...) memcpy(malloc(sizeof(t)), &(t){__VA_ARGS__}, sizeof(t))
#define expr(F, LOC,...) new(struct expr, .loc = LOC, .form = F, __VA_ARGS__)

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct loc { char *fn; int ln, col; } loc;

typedef struct value {
    enum { INT, CHAR, STRING, TUPLE, DATA, FN, PRIM } form;
    union {
        int     integer;
        unsigned character;
        char    *string;
        struct  { int n; struct value *tuple; };
        struct  { char *con; struct value *data; };
        struct  { char *param; struct expr *body; struct environ *env; };
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
        EAPP, ECASE, ELET, EREC, ETY, EAS,
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
        struct { char *id; expr *value; } as, fn;
    };
    loc loc;
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

typedef struct con_use {
    char *id;
    struct con_use *next;
} con_use;

typedef enum {
    // Keep in order with `tokens[]`.
    // Keep relative order: CID, single-char punct, ID, reserved
    TEOF, TINT, TCHAR, TSTRING, TCID, TLP, TRP, TLB, TRB,
    TCOMMA, TSEMI, TID, TEQUAL, TLET, TREC, TAND, TIN, TFN,
    TARROW, TCASE, TBAR, TIF, TTHEN, TELSE, TINFIXL, TINFIXR,
    TDATATYPE, TTYPING, TCONT, TAS,
} Token;

char *opchr = "!%&$*+-/:<=>?@\\~`^|";
char *tokens[] = {"eof", "int", "character", "string", "con id",
    "(", ")", "[", "]", ",", ";", "id", "=", "let", "rec",
    "and", "in", "fn", "->", "case", "|", "if", "then", "else",
    "infixl", "infixr", "datatype", "::", "---", "@", 0 };

char        source[128 * 1024];
char        *src;
char        *srcsol;
loc         srcloc;
Token       token;
bool        peeked;
int         tokint;
char        tokbuf[sizeof source];
char        *tokstr;
char        *interns[65536];
int         ninterns;
int         coin_types;
struct op   *optab;
types       *nongeneric;
typing      *typetab;
typing      *contab;
type        *inttype, *chartype, *strtype;
value       trueval, falseval;
char        *consid, *nilid, *noneid, *someid, *trueid, *falseid, *andid, *orid;
char        *refid, *ignore;
char        *strings[256];
int         formal_uid;

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

value function(char *param, expr *body, environ *env) {
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
                        return pr(") %s", t->id);
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

    case EFN:       return pr("fn %s -> %e", e->fn.id, e->fn.value);

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
    case EAS:       return pr("(%s @ %e)", e->as.id, e->as.value);
    }
}

void *print_escape(unsigned c, int quote) {
    switch (c) {
    case '\n':      return pr("\\n");
    case '\t':      return pr("\\t");
    default:        return pr(c == quote || c == '\\'? "\\%c": "%c", c);
    }
}

void *print_value(value x) {
    switch (x.form) {
    case INT:       return pr("%d", x.integer);
    case CHAR:      pr("'");
                    print_escape(x.character, '\'');
                    return pr("'");
    case STRING:    pr("\"");
                    for (char *s = x.string; *s; s++)
                        print_escape(*s, '"');
                    return pr("\"");
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
                    else if (x.data && x.data->form == DATA && x.data->data)
                        return pr("%s(%x)", x.con, *x.data);
                    else if (x.data)
                        return pr("%s %x", x.con, *x.data);
                    else
                        return pr("%s", x.con);

    case FN:        return pr("(#fn %s:%d:%d)",
                        x.body->loc.fn, x.body->loc.ln, x.body->loc.col);

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

con_use *cons_for(type *type) {
    if (type->form != TY)
        return 0;
    con_use *cons = 0;
    for (typing *i = contab; i; i = i->next) {
        struct type *t = i->type;
        if (t->form == TY && t->id == type->id ||
            isfn(t) && t->args[1]->form == TY && t->args[1]->id == type->id)
        {
            cons = new(con_use, i->id, cons);
        }
    }
    return cons;
}

void use_con(con_use **cons, char *id) {
    if (id == ignore)
        *cons = 0;
    else {
        con_use **i = cons;
        while (*i)
            if ((*i)->id == id)
                *i = (*i)->next;
            else
                i = &(*i)->next;
    }
}

_Noreturn void *syntax(char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("error %s:%d:%d: %*.\n", srcloc.fn, srcloc.ln, srcloc.col, msg, &ap);
    exit(1);
}

void open_source(char *filename) {
    srcloc = (loc) { strdup(filename), 1 };
    src = srcsol = source, peeked = false;
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
        if (*src == '\n') {
            srcloc.ln++;
            srcsol = src + 1;
        }
        else if (*src == '#')
            while (src[1] && src[1] != '\n')
                src++;

    srcloc.col = src - srcsol + 1;
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

expr *conref(char *id, loc loc) {
    return expr(ECON, loc, .id = id);
}

expr *binapp(bool iscon, char *id, expr *lhs, expr *rhs, loc loc) {
    expr *f = expr(iscon? ECON: EVAR, loc, .id = id);
    expr *x = expr(ETUPLE, loc, .n = 2, .tuple = new(expr*[2], lhs, rhs));
    return expr(EAPP, lhs->loc, .lhs = f, .rhs = x);
}

expr *ifmatch(expr *test, expr *affirm, expr *negative, loc loc) {
    struct rule *rules =
        new(struct rule, conref(falseid, negative->loc), negative,
        new(struct rule, conref(trueid, affirm->loc), affirm, 0));
    return expr(ECASE, loc, .subject = test, .rules = rules);
}

expr *listexp(void) {
    if (want(TRB))
        return conref(nilid, srcloc);

    expr *hd = expression();

    if (want(TCOMMA)) {
        loc loc = (peek(0), srcloc);
        expr *tl = listexp();
        return binapp(true, consid, hd, tl, loc);
    } else {
        need(TRB);
        return binapp(true, consid, hd, conref(nilid, srcloc), srcloc);
    }
}

expr *fnexp(Token delim, Token cont, char *cont_name, loc loc) {
    if (want(delim))
        return expression();

    type    *typing = want(TTYPING)? ty_with_coining(): 0;

    struct rule *rules = 0;
    struct rule **rulep = &rules;
    int     nformals = 0;
    int     uid = formal_uid;

    if (typing && peek(cont))
        goto check_cont;

    while (true) {
        struct loc loc = (peek(0), srcloc);
        int     n = 0;
        expr    **pars = 0;
        expr    *par;

        while ((par = atexp(false))) {
            pars = realloc(pars, (n + 1) * sizeof *pars);
            pars[n++] = par;
        }

        if (!rules) {
            nformals = n;
            formal_uid += n;
        }

        need(delim);
        expr *lhs = n == 1? pars[0]: expr(ETUPLE, loc, .n = n, .tuple = pars);
        expr *rhs = expression();

        if (n != nformals)
            syntax("arity does not match: %d/%d", n, nformals);
        *rulep = new(struct rule, lhs, rhs, *rulep);
        rulep = &(*rulep)->next;

check_cont:
        if (!want(cont))
            break;
        if (cont_name && (need(TID), strcmp(tokstr, cont_name)))
            syntax("need name after ---: %s", cont_name);
    }

    formal_uid -= nformals;

    expr **formals = malloc(nformals * sizeof(expr*));
    for (int i = 0; i < nformals; i++) {
        char id[8];
        sprintf(id, "'%d", uid++);
        formals[i] = expr(EVAR, loc, .id = intern(id));
    }

    expr *tuple = nformals == 1? formals[0]:
                  expr(ETUPLE, loc, .n = nformals, .tuple = formals);
    expr *result = expr(ECASE, loc, .subject = tuple, .rules = rules);


    for (int i = nformals; i-- > 0; )
        result = expr(EFN, loc, .fn = { formals[i]->id, result });
    return result;
}

expr *atexp(bool required) {
    loc loc = (peek(0), srcloc);

    // Do not consume operator as argument.
    if (!required && op_if_next(-1))
        return 0;

    if (want(TINT))     return expr(EINT, loc, .integer = tokint);
    if (want(TCHAR))    return expr(ECHAR, loc, .character = tokint);
    if (want(TSTRING))  return expr(ESTRING, loc, .string = tokstr);
    if (want(TCID))     return expr(ECON, loc, .id = tokstr);
    if (want(TLB))      return listexp();
    if (want(TFN))      return fnexp(TARROW, TBAR, 0, loc);

    if (want(TID)) {
        char *id = tokstr;
        if (want(TAS))
            return expr(EAS, loc, .as = {id, atexp(true)});
        return expr(EVAR, loc, .id = id);
    }
    if (want(TLP)) { // Tuple.
        int     n = 0;
        expr    **tuple = 0;

        do {
            if (peek(TRP)) break;
            tuple = realloc(tuple, (n + 1) * sizeof *tuple);
            tuple[n++] = expression();
        } while (want(TCOMMA));
        need(TRP);

        if (n == 1) {
            tuple[0]->loc = loc;
            return tuple[0];
        } else
            return expr(ETUPLE, loc, .n = n, .tuple = tuple);
    }

    return required? syntax("need expression before %s", tokens[token]): 0;
}

expr *appexp(void) {
    expr *lhs = atexp(true);
    expr *rhs;
    while ((rhs = atexp(false)))
        lhs = expr(EAPP, lhs->loc, .lhs = lhs, .rhs = rhs);
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
            lhs = op->id == andid?
                    ifmatch(lhs, rhs, conref(falseid, lhs->loc), lhs->loc):
                  op->id == orid?
                    ifmatch(lhs, conref(trueid, lhs->loc), rhs, lhs->loc):
                  binapp(op->iscon, op->id, lhs, rhs, lhs->loc);
        }
        return lhs;
    }
}

struct dec *rec_decs(void) {
    struct dec *decs = 0;
    struct dec **indir = &decs;

    want(TAND);
    do {
        loc     loc = (peek(0), srcloc);
        char    *id = (need(TID), tokstr);
        expr    *body = fnexp(TEQUAL, TCONT, id, loc);

        *indir = new(struct dec, id, body, 0);
        indir = &(*indir)->next;

    } while (want(TAND));

    return decs;
}

expr **nonrec_decs(expr **indir) {
    want(TAND);
    do {
        loc     loc = (peek(0), srcloc);
        expr    *lhs = atexp(true);
        char    *id = lhs->form==EVAR? lhs->id: "(bad)";
        expr    *rhs = fnexp(TEQUAL, TCONT, id, loc);

        *indir = expr(ELET, loc, .let = { lhs, rhs, 0 });
        indir = &(*indir)->let.body;

    } while (want(TAND));

    return indir;
}

expr *complex(void) {
    loc loc = (peek(0), srcloc);

    if (want(TLET))
        if (want(TREC)) {
            struct dec *decs = rec_decs();
            need(TIN);
            expr *body = expression();
            return expr(EREC, loc, .rec = { decs, body });
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

        return expr(ECASE, loc, .subject = subject, .rules = rules);
    }

    if (want(TIF)) {
        expr *test = expression();
        expr *affirm = (need(TTHEN), expression());
        expr *negative = (need(TELSE), expression());
        return ifmatch(test, affirm, negative, loc);
    }

    return infexp(0);
}

expr *expression(void) {
    expr *lhs = complex();

    if (want(TSEMI)) {
        expr    *body = expression();
        expr    *_ = expr(EVAR, srcloc, .id = ignore);
        return expr(ELET, lhs->loc, .let = { _, lhs, body });
    }

    else if (want(TTYPING)) {
        type *type = ty_with_coining();
        return expr(ETY, lhs->loc, .typing = { lhs, type });
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

    if (peek(TCID) || peek(TBAR)) { // Define datatype.
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
    while (!peek(TEOF)) {
        loc loc = (peek(0), srcloc);
        if (want(TINFIXL)) infixdec(1);
        else if (want(TINFIXR)) infixdec(0);
        else if (want(TDATATYPE)) datbind();
        else {
            need(TLET);
            if (want(TREC)) {
                struct dec *decs = rec_decs();
                *nextp = expr(EREC, loc, .rec = { decs, 0 });
                nextp = &(*nextp)->rec.body;
            }
            else
                nextp = nonrec_decs(nextp);
        }
    }

    if (*nextp == 0)
        *nextp = expr(ETUPLE, srcloc);
    return nextp;
}

_Noreturn void *semantic(expr *c, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("error %s:%d:%d: %*.\n", c->loc.fn, c->loc.ln, c->loc.col, msg, &ap);
    exit(1);
}

void *warn(expr *c, char *msg, ...) {
    va_list ap; va_start(ap, msg);
    pr("warning %s:%d:%d: %*.\n", c->loc.fn, c->loc.ln, c->loc.col, msg, &ap);
    va_end(ap);
}

type *unify(expr *c, type *want, type *got) {
    if (!unifies(want, got))
        semantic(c, "type error:\nwant: %t\ngot : %t", want, got);

    want = target(want);
    got = target(got);
    return want->form == ALIASTY? want:
           got->form == ALIASTY? got:
           want;
}

bool is_value(expr *e) {
    switch (e->form) {
    case EINT:
    case ECHAR:
    case ESTRING:
    case EVAR:
    case ECON:
    case EFN:
        return true;

    case ECASE:
    case ELET:
    case EREC:
    case EAS:
        return false;

    case ETUPLE:    for (int i = 0; i < e->n; i++)
                        if (!is_value(e->tuple[i]))
                            return false;
                    return true;

    case EAPP:      return  e->lhs->form == ECON &&
                            e->lhs->id != refid &&
                            is_value(e->rhs);

    case ETY:       return is_value(e->typing.value);
    }
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
    con_use *con_use;

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

    case EFN:       lhs = tyvar(0);
                    nongeneric = new(types, lhs, nongeneric);
                    if (c->fn.id != ignore)
                        env = new(typing, c->fn.id, lhs, env);
                    rhs = check(c->fn.value, env);
                    nongeneric = oldng;
                    return fntype(lhs, rhs);

    case EAPP:      lhs = check(c->lhs, env);
                    rhs = check(c->rhs, env);

                    lhs = target(lhs);

                    if (isfn(lhs)) { // Cleaner error with only arg.
                        unify(c->rhs, lhs->args[0], rhs);
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

                    con_use = cons_for(target(lhs));
                    for (struct rule *i = c->rules; i; i = i->next)
                        switch (i->lhs->form) {
                        case EVAR: use_con(&con_use, ignore); break;
                        case ECON: use_con(&con_use, i->lhs->id); break;
                        case EAPP: use_con(&con_use, i->lhs->lhs->id); break;
                        default: break;
                        }
                    for (struct con_use *i = con_use; i; i = i->next)
                        warn(c, "unhandled case: %s", i->id);

                    return rhs;

    case ELET:      if (c->let.rhs->form == EFN && c->let.lhs->form != EVAR)
                        semantic(c->let.lhs, "function not bound to a var");
                    rhs = check(c->let.rhs, env);
                    lhs = check_pattern(c->let.lhs, &env);
                    unify(c->let.lhs, lhs, rhs);
                    if (is_value(c->let.rhs)) // Look up "value restriction".
                        nongeneric = oldng;
                    type = check(c->let.body, env);
                    nongeneric = oldng;
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

    case EAS:       semantic(c, "@ cannot be used as an expression");
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

    case EAS:       type = tyvar(0);
                    nongeneric = new(types, type, nongeneric);
                    if (c->as.id != ignore)
                        *env = new(typing, c->as.id, type, *env);
                    return unify(c, type, check_pattern(c->as.value, env));

    case EFN:
    case ECASE:
    case ELET:
    case EREC:
        return semantic(c, "invalid pattern");
    }
}

bool bind_pattern(expr *pat, value x, environ **env) {
    switch (pat->form) {
    case EINT:      return pat->integer == x.integer;
    case ECHAR:     return pat->character == x.character;
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

    case ETY:       return bind_pattern(pat->typing.value, x, env);

    case EAS:       if (pat->id != ignore)
                        *env = new(environ, pat->as.id, x, *env);
                    return bind_pattern(pat->as.value, x, env);

    case EFN:
    case ECASE:
    case ELET:
    case EREC:
        semantic(pat, "INTERNAL ERROR: PATTERN (%e)", pat);
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

    case EFN:       return function(code->fn.id, code->fn.value, env);

    case EAPP:      x = eval(code->lhs, env);
                    y = eval(code->rhs, env);

                    if (x.form == PRIM)
                        return x.prim(code, y);

                    if (x.form == DATA)
                        return data(x.con, new(value[1], y));

                    if (x.form == FN) {
                        env = new(environ, x.param, y, x.env);
                        code = x.body;
                        goto tail;
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
                                  function(i->fn->fn.id, i->fn->fn.value, 0),
                                  env);

                    for (environ *i = env; i != saved; i = i->next)
                        i->value.env = env;

                    code = code->rec.body;
                    goto tail;

    case ETY:       code = code->typing.value;
                    goto tail;

    case EAS:       semantic(code, "INTERNAL ERROR: EAS: %c", code);
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
    if (x.form == STRING)
        pr("%s", x.string);
    else
        pr("%x", x);
    return x;
}
value prim_ord(expr *code, value x) {
    return integer(x.character);
}
value prim_chr(expr *code, value x) {
    return character(x.integer & 255);
}
value prim_chrstr(expr *code, value x) {
    return string(strings[x.character]);
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
value prim_join(expr *code, value x) {
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
    setvbuf(stdout, 0, _IONBF, 0);

    typing      *cenv = 0;
    environ     *renv = 0;

    // Set up values the compiler needs.
    consid      = intern(":");
    nilid       = intern("NIL");
    noneid      = intern("NONE");
    someid      = intern("SOME");
    trueid      = intern("TRUE");
    falseid     = intern("FALSE");
    refid       = intern("REF");
    andid       = intern("&&");
    orid        = intern("||");
    ignore      = intern("_");
    falseval    = data(intern("FALSE"), 0);
    trueval     = data(intern("TRUE"), 0);
    inttype     = tycon(intern("int"), 0, 0);
    chartype    = tycon(intern("char"), 0, 0);
    strtype     = tycon(intern("string"), 0, 0);
    typetab     = new(typing, inttype->id, inttype, typetab);
    typetab     = new(typing, chartype->id, chartype, typetab);
    typetab     = new(typing, strtype->id, strtype, typetab);
    for (int i = 0; i < 255; i++)
        strings[i] = intern((char[]){ i, 0 });

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
    basis("chrstr", chartype, strtype, prim_chrstr, &cenv, &renv);
    basis("sub", strint, chartype, prim_sub, &cenv, &renv);
    basis("substring", strint2, strtype, prim_substring, &cenv, &renv);
    basis("size", strtype, inttype, prim_size, &cenv, &renv);
    basis("implode", charlist, strtype, prim_implode, &cenv, &renv);
    basis("join", strlist, strtype, prim_join, &cenv, &renv);
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
