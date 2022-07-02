#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/*
- commas
- namespaced maps #:person{:first "Han"} (tag or prefix)
- #my.klass_or_type_or_record[:a :b :c] (tag)
- #my.record{:a 1, :b 2} (tag)
- @form (prefix)
- ^String (^ is prefix, whole thing is tag?)
- #'test
- #_[2]
- ~, ~@ `
- #?, #?@
*/

#define ASIZE(a) (sizeof(a) / sizeof(a[0]))

const int TRIE_CHILDREN_NUM = '~' - '!' + 1;

typedef struct trie_node {
    struct trie_node *children[TRIE_CHILDREN_NUM];
    const char *rest;
    bool is_terminal;
} trie_node;

trie_node *make_trie_node(void) {
    trie_node *res = malloc(sizeof(trie_node));
    memset(res, 0, sizeof(trie_node));
    return res;
}

bool is_in_trie(trie_node *root, const char *start, const char *end) {
    trie_node *node = root;
    for (const char *p = start; p != end; p++) {
        int index = p[0] - '!';
        if (node->rest && !strcmp(node->rest, p))
            return true;
        if (!node->children[index])
            return false;
        node = node->children[index];
    }
    return node->is_terminal || (node->rest && !node->rest[0]);
}

void add_to_trie(trie_node *node, const char *data) {
    int index = data[0] - '!';
    if (!node->children[index]) {
        node->children[index] = make_trie_node();
        node->children[index]->rest = data + 1;
        const char *rest = node->rest;
        if (rest) {
            node->rest = NULL;
            if (!rest[0]) {
                node->is_terminal = true;
            } else {
                add_to_trie(node, rest);
            }
        }
        return;
    }
    if (!data[1]) {
        node->children[index]->is_terminal = true;
    } else {
        add_to_trie(node->children[index], data + 1);
    }
}

void print_trie(trie_node *node, int indent) {
    for (int k = 0; k < indent; k++) {
        putchar(' ');
    }
    if (node->rest) {
        printf("%d: %s\n", node->is_terminal, node->rest);
    } else {
        printf("%d\n", node->is_terminal);
        for (int i = 0; i < TRIE_CHILDREN_NUM; i++) {
            if (node->children[i]) {
                for (int k = 0; k < indent; k++) {
                    putchar(' ');
                }
                printf("%c: ", i + '!');
                print_trie(node->children[i], indent + 3);
            }
        }
    }
}

typedef struct {
    char *input;
    char *output;
} options;

typedef enum {
    V_CHAR,
    V_STRING,
    V_REGEX,
    V_NUMBER,
    V_SYMBOL,
    V_KEYWORD,
    V_NIL,
    V_BOOL,
    V_COMMENT,
    V_LIST,
    V_VECTOR,
    V_MAP,
    V_SET,
    V_FN,
    V_UKNOWN
} value_type;

char *value_type_names[V_UKNOWN] = {
    "char", "string",  "regex", "number", "symbol", "keyword", "nil",
    "bool", "comment", "list",  "vector", "map",    "set",     "fn"};

typedef struct {
    value_type type;
    char *start;
    char *end;
    char *prefix;
    int new_lines;
} value;

typedef struct {
    value val;
    value **vals;
    int count;
    int capacity;
} collection;

typedef struct {
    value *last_read_val;
    size_t size;
    char *bytes;
    char *sp;
    int indent;
    int offset;
} context;

value *read_value(void);
bool is_separator(char c);
void format_value(value *val, FILE *f);

context *ctx;
const char *indent_one[] = {"bound-fn",     "if",           "if-not",
                            "case",         "cond",         "cond->",
                            "cond->>",      "as->",         "condp",
                            "when",         "while",        "when-not",
                            "when-first",   "do",           "future",
                            "thread", // with-
                            "comment",      "doto",         "locking",
                            "proxy",        "reify",        "fdef",
                            "defprotocol",  "extend",       "extend-protocol",
                            "extend-type",  "catch",        "let",
                            "letfn",        "binding",      "loop",
                            "for",          "go-loop",      "doseq",
                            "dotimes",      "when-let",     "if-let",
                            "defstruct",    "struct-map",   "defmethod",
                            "testing",      "are",          "deftest",
                            "context",      "use-fixtures", "POST",
                            "GET",          "PUT",          "DELETE",
                            "handler-case", "handle",       "dotrace",
                            "deftrace",     "match"};

trie_node *indent_one_trie;

static inline size_t length(value *val) { return val->end - val->start; }

void pump_tries(void) {
    indent_one_trie = make_trie_node();
    for (int i = 0; i < ASIZE(indent_one); i++) {
        add_to_trie(indent_one_trie, indent_one[i]);
    }
}

context *make_context(void) {
    context *ctx = malloc(sizeof(context));
    memset(ctx, 0, sizeof(context));
    return ctx;
}

value *make_value(value_type type, char *start, char *end) {
    value *res = malloc(sizeof(value));
    res->type = type;
    res->start = start;
    res->end = end;
    res->prefix = NULL;
    res->new_lines = 0;
    return res;
}

collection *make_collection(value_type type, char *start, char *prefix) {
    collection *res = malloc(sizeof(collection));
    memset(res, 0, (sizeof(collection)));
    res->val.type = type;
    res->val.start = start;
    res->val.prefix = prefix;
    return res;
}

bool is_collection(value *val) { return val->type >= V_LIST; }

void add_to_coll(collection *coll, value *v) {
    if (coll->count == coll->capacity) {
        coll->capacity = coll->capacity ? coll->capacity * 2 : 4;
        coll->vals = realloc(coll->vals, coll->capacity * sizeof(value *));
    }
    coll->vals[coll->count++] = v;
}

void print_usage(void) {
    fprintf(stderr, "Usage: cljf <filename> [-o <filename>]\n");
    exit(1);
}

void print_value(value *val) {
    printf("%s[%ld]: %.*s\n", value_type_names[val->type], length(val),
           (int)(length(val)), val->start);
    if (is_collection(val)) {
        collection *coll = (collection *)val;
        for (int i = 0; i < coll->count; i++) {
            print_value(coll->vals[i]);
        }
    }
}

value *read_char(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_CHAR, start, ctx->sp);
}

value *read_string(value_type type, char *start) {
    bool slash = false;
    for (ctx->sp++; *ctx->sp != '"' || slash; ctx->sp++) {
        switch (*ctx->sp) {
        case '\0':
            fprintf(stderr, "Unexpected end of file while reading a string\n");
            exit(4);
            break;
        case '\\':
            slash = !slash;
            break;
        default:
            slash = false;
            break;
        }
    }
    ctx->sp++;
    return make_value(type, start, ctx->sp);
}

bool is_whitespace(char c) {
    // Note: doesn't handle unicode whitespaces (yet?).
    switch (c) {
    case ' ':
    case '\t':
    case '\n':
    case '\f':
    case '\r':
    case '\v':
    case '\x1C':
    case '\x1D':
    case '\x1E':
    case '\x1F':
        return true;
    default:
        return false;
    }
}

bool is_separator(char c) {
    if (is_whitespace(c))
        return true;
    switch (c) {
    case '\0':
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case ',':
        return true;
    default:
        return false;
    }
}

value *read_number(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_NUMBER, start, ctx->sp);
}

value *read_comment(void) {
    char *start = ctx->sp;
    for (ctx->sp++; *ctx->sp != '\n'; ctx->sp++)
        ;
    return make_value(V_COMMENT, start, ctx->sp);
}

value *read_symbol(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    value_type t = V_SYMBOL;
    size_t len = ctx->sp - start;
    if (len == 3 && start[0] == 'n' && start[1] == 'i' && start[2] == 'l') {
        t = V_NIL;
    } else if (len == 4 && start[0] == 't' && start[1] == 'r' &&
               start[2] == 'u' && start[3] == 'e') {
        t = V_BOOL;
    } else if (len == 5 && start[0] == 'f' && start[1] == 'a' &&
               start[2] == 'l' && start[3] == 's' && start[4] == 'e') {
        t = V_BOOL;
    } else if (len == 5 && start[0] == '#' && start[1] == '#' &&
               start[2] == 'N' && start[3] == 'a' && start[4] == 'N') {
        t = V_NUMBER;
    } else if (len == 5 && start[0] == '#' && start[1] == '#' &&
               start[2] == 'I' && start[3] == 'n' && start[4] == 'f') {
        t = V_NUMBER;
    } else if (len == 6 && start[0] == '#' && start[1] == '#' &&
               start[2] == '-' && start[3] == 'I' && start[4] == 'n' &&
               start[5] == 'f') {
        t = V_NUMBER;
    }
    return make_value(t, start, ctx->sp);
}

value *read_keyword(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_KEYWORD, start, ctx->sp);
}

void skip_whitespace(void) {
    while (is_whitespace(*ctx->sp)) {
        if (ctx->last_read_val && *ctx->sp == '\n') {
            ctx->last_read_val->new_lines++;
        }
        ctx->sp++;
    }
}

bool is_digit(char c) { return c >= '0' && c <= '9'; }

value *read_collection(value_type type, char *start, char end, char *prefix) {
    ctx->sp++;
    collection *coll = make_collection(type, start, prefix);
    skip_whitespace();
    while (*ctx->sp != end) {
        ctx->last_read_val = read_value();
        if (!ctx->last_read_val) {
            fprintf(stderr,
                    "Unexpected end of file while reading a collection\n");
            exit(4);
        }
        add_to_coll(coll, ctx->last_read_val);
        skip_whitespace();
    }
    ctx->sp++;
    coll->val.end = ctx->sp;
    return (value *)coll;
}

value *read_value(void) {
    skip_whitespace();
    if (!*ctx->sp)
        return NULL;
    switch (*ctx->sp) {
    case '(':
        return read_collection(V_LIST, ctx->sp, ')', NULL);
    case '[':
        return read_collection(V_VECTOR, ctx->sp, ']', NULL);
    case '{':
        return read_collection(V_MAP, ctx->sp, '}', NULL);
    case '"':
        return read_string(V_STRING, ctx->sp);
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        return read_number();
    case '.':
        if (is_digit(ctx->sp[1]))
            return read_number();
        return read_symbol();
    case ':':
        return read_keyword();
    case '\\':
        return read_char();
    case ';':
        return read_comment();
    case '#':
        switch (ctx->sp[1]) {
        case '{':
            ctx->sp++;
            return read_collection(V_SET, ctx->sp - 1, '}', "#");
        case '(':
            ctx->sp++;
            return read_collection(V_FN, ctx->sp - 1, ')', "#");
        case '"':
            ctx->sp++;
            return read_string(V_REGEX, ctx->sp - 1);
        }
        // fallthrough
    default:
        return read_symbol();
    }
}

bool check_rest(int start, int len, value *val, const char *rest) {
    return val->end - (val->start + start) == len &&
           memcmp(val->start + start, rest, len) == 0;
}

bool is_indent_one(value *val) {
    return is_in_trie(indent_one_trie, val->start, val->end);
}

void format_collection(collection *coll, char start, char end, FILE *f) {
    int old_indent = ctx->indent;
    if (coll->val.prefix) {
        fputs(coll->val.prefix, f);
        ctx->offset += strlen(coll->val.prefix);
    }
    fputc(start, f);
    ctx->offset++;
    ctx->indent = ctx->offset;

    if (start == '(' && coll->count > 1 && coll->vals[0]->type == V_SYMBOL) {
        if (is_indent_one(coll->vals[0])) {
            ctx->indent++;
        } else {
            ctx->indent += (coll->vals[0]->end - coll->vals[0]->start + 1);
        }
    }

    for (int i = 0; i < coll->count - 1; i++) {
        value *val = coll->vals[i];

        format_value(val, f);

        for (int j = 0; j < val->new_lines; j++) {
            fputc('\n', f);
        }
        if (val->new_lines) {
            for (int k = 0; k < ctx->indent; k++) {
                fputc(' ', f);
            }
            ctx->offset = ctx->indent;
        } else {
            fputc(' ', f);
            ctx->offset++;
        }
    }
    if (coll->count) {
        format_value(coll->vals[coll->count - 1], f);
    }
    fputc(end, f);
    ctx->offset++;

    ctx->indent = old_indent;
}

void format_value(value *val, FILE *f) {
    switch (val->type) {
    case V_LIST:
        format_collection((collection *)val, '(', ')', f);
        break;
    case V_VECTOR:
        format_collection((collection *)val, '[', ']', f);
        break;
    case V_MAP:
        format_collection((collection *)val, '{', '}', f);
        break;
    case V_SET:
        format_collection((collection *)val, '{', '}', f);
        break;
    case V_FN:
        format_collection((collection *)val, '(', ')', f);
        break;
    default: {
        size_t len = length(val);
        fwrite(val->start, 1, len, f);
        ctx->offset += len;
        break;
    }
    }
}

void read_file(const char *filename, context *ctx) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Cannot open file: %s\n", filename);
        exit(2);
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    ctx->size = size;
    ctx->bytes = malloc(size + 1);
    ctx->sp = ctx->bytes;
    if (1 != fread(ctx->bytes, size, 1, f)) {
        fprintf(stderr, "Error reading file: %s.", filename);
        exit(3);
    }
    fclose(f);
    ctx->bytes[size] = '\0';
}

void parse_args(int argc, char **argv, options *opts) {
    if (argc == 2) {
        opts->input = argv[1];
        opts->output = NULL;
        return;
    }

    if (argc == 4) {
        if (!strcmp(argv[1], "-o")) {
            opts->input = argv[3];
            opts->output = argv[2];
            return;
        }
        if (!strcmp(argv[2], "-o")) {
            opts->input = argv[1];
            opts->output = argv[3];
            return;
        }
    }

    print_usage();
}

int main(int argc, char **argv) {
    options opts;
    parse_args(argc, argv, &opts);

    pump_tries();

    ctx = make_context();
    read_file(opts.input, ctx);

    FILE *out = stdout;

    if (opts.output) {
        out = fopen(opts.output, "wb");
        if (!out) {
            fprintf(stderr, "Cannot open file for writing: %s\n", opts.output);
            exit(2);
        }
    }

    while ((ctx->last_read_val = read_value())) {
        skip_whitespace();
        ctx->offset = 0;
        ctx->indent = 0;
        format_value(ctx->last_read_val, out);
        for (int j = 0; j < ctx->last_read_val->new_lines; j++) {
            fputc('\n', out);
        }
    }
    fclose(out);
}
