#include <dirent.h>
#include <errno.h>
#include <fts.h>
#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define ASIZE(a) (sizeof(a) / sizeof(a[0]))
#define MAX_PATH_SIZE 2048

const char *body_indent[] = {
    "fn",      "bound-fn", "case",    "cond->",       "cond->>",
    "as->",    "condp",    "while",   "future",       "thread",
    "comment", "doto",     "locking", "fdef",         "extend",
    "catch",   "let",      "letfn",   "binding",      "loop",
    "for",     "go-loop",  "doseq",   "dotimes",      "struct-map",
    "testing", "are",      "context", "use-fixtures", "POST",
    "GET",     "PUT",      "DELETE",  "handler-case", "handle",
    "dotrace", "match"};

const char *do_indent[] = {"do",   "try",   "finally", "go",
                           "alt!", "alt!!", "cond"};

const int TRIE_CHILDREN_NUM = '~' - '!' + 1;

typedef struct trie_node {
    struct trie_node *children[TRIE_CHILDREN_NUM];
    const char *rest;
    bool is_terminal;
} trie_node;

static trie_node *make_trie_node(void) {
    trie_node *res = malloc(sizeof(trie_node));
    memset(res, 0, sizeof(trie_node));
    return res;
}

static bool is_in_trie(trie_node *root, const char *start, const char *end) {
    trie_node *node = root;
    for (const char *p = start; p != end; p++) {
        int index = p[0] - '!';
        if (node->rest && !memcmp(node->rest, p, end - p) &&
            !node->rest[end - p])
            return true;
        if (!node->children[index])
            return false;
        node = node->children[index];
    }
    return node->is_terminal || (node->rest && !node->rest[0]);
}

static void add_to_trie(trie_node *node, const char *data) {
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
    V_MAX
} value_type;

char *value_type_names[V_MAX] = {
    "char", "string",  "regex", "number", "symbol", "keyword", "nil",
    "bool", "comment", "list",  "vector", "map",    "set",     "fn"};

typedef struct {
    value_type type;
    char *start;
    char *end;
    char *name_start;
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
    const char *filename;
    int indent;
    int offset;
    bool is_defrecord;
} context;

static value *read_value(void);
static bool is_separator(char c);
static void format_value(value *val, FILE *f);

context *ctx;

trie_node *body_indent_trie;
trie_node *do_indent_trie;

static inline size_t length(value *val) { return val->end - val->start; }

static void pump_tries(void) {
    body_indent_trie = make_trie_node();
    for (int i = 0; i < ASIZE(body_indent); i++) {
        add_to_trie(body_indent_trie, body_indent[i]);
    }
    do_indent_trie = make_trie_node();
    for (int i = 0; i < ASIZE(do_indent); i++) {
        add_to_trie(do_indent_trie, do_indent[i]);
    }
}

static context *make_context(void) {
    context *ctx = malloc(sizeof(context));
    memset(ctx, 0, sizeof(context));
    return ctx;
}

static value *make_value(value_type type, char *start, char *end) {
    value *res = malloc(sizeof(value));
    res->type = type;
    res->start = start;
    res->name_start = start;
    res->end = end;
    res->new_lines = 0;
    return res;
}

static collection *make_collection(value_type type, char *start) {
    collection *res = malloc(sizeof(collection));
    memset(res, 0, (sizeof(collection)));
    res->val.type = type;
    res->val.start = start;
    return res;
}

static void add_to_coll(collection *coll, value *v) {
    if (coll->count == coll->capacity) {
        coll->capacity = coll->capacity ? coll->capacity * 2 : 4;
        coll->vals = realloc(coll->vals, coll->capacity * sizeof(value *));
    }
    coll->vals[coll->count++] = v;
}

static void print_usage(void) {
    fprintf(stderr,
            "Usage: cljf [<input file or directory>] [-o <output file>]\n"
            "Examples:\n"
            "cljf                    - read source code from stdin and write "
            "formatted "
            "code to stdout\n"
            "cljf foo.clj            - format file foo.clj (override its "
            "content with formatted code)\n"
            "cljf foo.clj -o bar.clj - read source code from file "
            "foo.clj and write formatted code to file bar.clj\n"
            "cljf src                - format all Clojure files (files with "
            "extensions "
            "*.clj, *.cljs, *.cljc, *.joke) in src directory\n");
    exit(1);
}

static value *read_char(void) {
    char *start = ctx->sp;
    if (!ctx->sp[1]) {
        fprintf(stderr, "Unexpected end of file while reading a char\n");
        fprintf(stderr, "File: %s\n", ctx->filename);
        exit(4);
    }
    for (ctx->sp += 2; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_CHAR, start, ctx->sp);
}

static value *read_string(value_type type, char *start) {
    bool slash = false;
    for (ctx->sp++; *ctx->sp != '"' || slash; ctx->sp++) {
        switch (*ctx->sp) {
        case '\0':
            fprintf(stderr, "Unexpected end of file while reading a string\n");
            fprintf(stderr, "File: %s\n", ctx->filename);
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

static bool is_whitespace(char c) {
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

static bool is_separator(char c) {
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
    case '"':
    case ';':
    case '\\':
        return true;
    default:
        return false;
    }
}

static value *read_number(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_NUMBER, start, ctx->sp);
}

static value *read_comment(void) {
    char *start = ctx->sp;
    for (ctx->sp++; *ctx->sp != '\n' && *ctx->sp; ctx->sp++)
        ;
    return make_value(V_COMMENT, start, ctx->sp);
}

static value *read_symbol(void) {
    char *start = ctx->sp;
    char *name_start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++) {
        if ((*ctx->sp) == '/') {
            name_start = ctx->sp + 1;
        }
    }
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
    value *res = make_value(t, start, ctx->sp);
    res->name_start = name_start;
    return res;
}

static value *read_keyword(void) {
    char *start = ctx->sp;
    for (ctx->sp++; !is_separator(*ctx->sp); ctx->sp++)
        ;
    return make_value(V_KEYWORD, start, ctx->sp);
}

static void skip_whitespace(void) {
    while (is_whitespace(*ctx->sp)) {
        if (ctx->last_read_val && *ctx->sp == '\n') {
            ctx->last_read_val->new_lines++;
        }
        ctx->sp++;
    }
}

static bool is_digit(char c) { return c >= '0' && c <= '9'; }

static value *read_collection(value_type type, char *start, char end) {
    ctx->sp++;
    collection *coll = make_collection(type, start);
    skip_whitespace();
    while (*ctx->sp != end) {
        ctx->last_read_val = read_value();
        if (!ctx->last_read_val) {
            fprintf(stderr,
                    "Unexpected end of file while reading a collection\n");
            fprintf(stderr, "File: %s\n", ctx->filename);
            exit(4);
        }
        add_to_coll(coll, ctx->last_read_val);
        skip_whitespace();
    }
    ctx->sp++;
    coll->val.end = ctx->sp;
    return (value *)coll;
}

static value *read_value(void) {
    skip_whitespace();
    if (!*ctx->sp)
        return NULL;
    switch (*ctx->sp) {
    case '(':
        return read_collection(V_LIST, ctx->sp, ')');
    case '[':
        return read_collection(V_VECTOR, ctx->sp, ']');
    case '{':
        return read_collection(V_MAP, ctx->sp, '}');
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
            return read_collection(V_SET, ctx->sp - 1, '}');
        case '(':
            ctx->sp++;
            return read_collection(V_FN, ctx->sp - 1, ')');
        case '"':
            ctx->sp++;
            return read_string(V_REGEX, ctx->sp - 1);
        case '_':
            ctx->sp += 2;
            return make_value(V_SYMBOL, ctx->sp - 2, ctx->sp);
        }
        // fallthrough
    default:
        return read_symbol();
    }
}

static inline bool is_body_indent(value *val) {
    return is_in_trie(body_indent_trie, val->name_start, val->end) ||
           (val->end - val->name_start > 5 &&
            !memcmp(val->name_start, "with-", 5));
}

static inline bool is_do_indent(value *val) {
    return is_in_trie(do_indent_trie, val->name_start, val->end);
}

static inline bool is_def_indent(value *val) {
    size_t len = val->end - val->name_start;
    return !memcmp(val->name_start, "def", 3) ||
           (len == 2 && (!memcmp(val->name_start, "if", 2) ||
                         !memcmp(val->name_start, "ns", 2))) ||
           (len > 3 && !memcmp(val->name_start, "if-", 3)) ||
           (len == 4 && !memcmp(val->name_start, "when", 4)) ||
           (len == 5 && (!memcmp(val->name_start, "reify", 5) ||
                         !memcmp(val->name_start, "proxy", 5))) ||
           (len == 11 && !memcmp(val->name_start, "extend-type", 11)) ||
           (len > 5 && !memcmp(val->name_start, "when-", 5)) ||
           (len == 15 && !memcmp(val->name_start, "extend-protocol", 15));
}

static inline bool is_require_indent(value *val) {
    size_t len = val->end - val->start;
    return (len == 8 && !memcmp(val->start, ":require", 8)) ||
           (len == 7 && !memcmp(val->start, ":import", 7));
}

static inline bool is_comma(value *val) {
    return val->type == V_SYMBOL && val->start[0] == ',' &&
           val->start + 1 == val->end;
}

static inline bool is_prefix(value *val) {
    if (val->type != V_SYMBOL)
        return false;
    switch (length(val)) {
    case 1:
        switch (val->start[0]) {
        case '~':
        case '`':
        case '^':
        case '\'':
        case '@':
            return true;
        default:
            return false;
        }
    case 2:
        return (val->start[0] == '#' &&
                (val->start[1] == '?' || val->start[1] == '_')) ||
               (val->start[0] == '~' && val->start[1] == '@');
    case 3:
        return val->start[0] == '#' &&
               ((val->start[1] == '?' && val->start[2] == '@') ||
                val->start[1] == ':');
    default:
        return val->start[0] == '#' && val->start[1] == ':';
    }
}

static bool require_less(value *v1, value *v2) {
    if (v1->type > V_COMMENT && ((collection *)v1)->count) {
        v1 = ((collection *)v1)->vals[0];
    }
    if (v2->type > V_COMMENT && ((collection *)v2)->count) {
        v2 = ((collection *)v2)->vals[0];
    }
    size_t len1 = length(v1);
    size_t len2 = length(v2);
    int r = memcmp(v1->start, v2->start, len1 < len2 ? len1 : len2);
    if (!r) {
        return len1 < len2;
    }
    return r < 0;
}

static void sort_require(collection *coll) {
    for (int i = 1; i < coll->count; i++) {
        switch (coll->vals[i]->type) {
        case V_VECTOR:
        case V_LIST:
            break;
        case V_SYMBOL:
            if (is_prefix(coll->vals[i]))
                return;
            break;
        default:
            return;
        }
        coll->vals[i]->new_lines = 1;
    }
    for (int i = 1; i < coll->count - 1; i++) {
        for (int j = i + 1; j < coll->count; j++) {
            if (require_less(coll->vals[j], coll->vals[i])) {
                value *t = coll->vals[i];
                coll->vals[i] = coll->vals[j];
                coll->vals[j] = t;
            }
        }
    }
}

static void format_collection(collection *coll, char start, char end, FILE *f) {
    int old_indent = ctx->indent;
    if (coll->val.type == V_FN || coll->val.type == V_SET) {
        fputc('#', f);
        ctx->offset++;
    }
    fputc(start, f);
    ctx->offset++;
    ctx->indent = ctx->offset;

    bool is_defrecord = false;
    bool old_is_defrecord = ctx->is_defrecord;

    if (start == '(' && coll->count > 1) {
        value *v = coll->vals[0];
        switch (v->type) {
        case V_SYMBOL:
            if (ctx->is_defrecord || is_body_indent(v) ||
                (is_do_indent(v) && v->new_lines)) {
                ctx->indent++;
            } else if (is_def_indent(v)) {
                ctx->indent++;
                v->new_lines = 0;
                size_t len = v->end - v->name_start;
                if ((len == 5 && (!memcmp(v->name_start, "reify", 5) ||
                                  !memcmp(v->name_start, "proxy", 5))) ||
                    (len == 9 && !memcmp(v->name_start, "defrecord", 9)) ||
                    (len == 11 &&
                     (!memcmp(v->name_start, "defprotocol", 11) ||
                      !memcmp(v->name_start, "extend-type", 11))) ||
                    (len == 15 &&
                     !memcmp(v->name_start, "extend-protocol", 15))) {
                    is_defrecord = true;
                }
            } else if (!v->new_lines && v->start[0] != '^') {
                ctx->indent += (v->end - v->start + 1);
            }
            break;
        case V_KEYWORD:
            if (is_require_indent(v)) {
                sort_require(coll);
                ctx->indent += (v->end - v->start + 1);
                v->new_lines = 0;
            } else if (!v->new_lines && coll->count == 3) {
                // Looks like a (:keyword map default) form,
                // format as a function call.
                ctx->indent += (v->end - v->start + 1);
            }
            break;
        default:
            break;
        }
    }

    ctx->is_defrecord = is_defrecord;

    for (int i = 0; i < coll->count - 1; i++) {
        value *val = coll->vals[i];

        format_value(val, f);

        if (is_prefix(val) || is_comma(coll->vals[i + 1]))
            continue;

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
        if (coll->vals[coll->count - 1]->type == V_COMMENT) {
            fputc('\n', f);
            for (int k = 0; k < ctx->indent; k++) {
                fputc(' ', f);
            }
        }
    }
    fputc(end, f);
    ctx->offset++;

    ctx->indent = old_indent;
    ctx->is_defrecord = old_is_defrecord;
}

static void format_value(value *val, FILE *f) {
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

#define CHUNK_SIZE 4096

static void read_stdin(context *ctx) {
    ctx->size = CHUNK_SIZE;
    ctx->bytes = malloc(ctx->size + 1);
    char *p = ctx->bytes;
    size_t cnt;
    while ((cnt = fread(p, 1, CHUNK_SIZE, stdin)) == CHUNK_SIZE) {
        ctx->size += CHUNK_SIZE;
        ctx->bytes = realloc(ctx->bytes, ctx->size + 1);
        p = ctx->bytes + ctx->size - CHUNK_SIZE;
    }
    ctx->size += cnt;
    ctx->bytes[ctx->size] = '\0';
    ctx->sp = ctx->bytes;
    ctx->filename = "stdin";
}

static void read_file(const char *filename, context *ctx) {
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
    ctx->filename = filename;
}

static void parse_args(int argc, char **argv, options *opts) {
    if (argc == 1) {
        opts->input = NULL;
        opts->output = NULL;
        return;
    } else if (argc == 2) {
        opts->input = argv[1];
        opts->output = argv[1];
        return;
    } else if (argc == 4) {
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

bool is_clj(const char *filename) {
    size_t len = strlen(filename);
    if (len < 5)
        return false;
    return (!strcmp(filename + (len - 4), ".clj") ||
            !strcmp(filename + (len - 5), ".cljs") ||
            !strcmp(filename + (len - 5), ".cljc") ||
            !strcmp(filename + (len - 4), ".edn") ||
            !strcmp(filename + (len - 5), ".cljd") ||
            !strcmp(filename + (len - 5), ".joke"));
}

void format_file(const char *input, const char *output) {
    ctx = make_context();

    if (input) {
        read_file(input, ctx);
    } else {
        read_stdin(ctx);
    }

    collection *forms = make_collection(V_VECTOR, NULL);

    while ((ctx->last_read_val = read_value())) {
        add_to_coll(forms, ctx->last_read_val);
    }

    FILE *out = stdout;

    if (output) {
        out = fopen(output, "wb");
        if (!out) {
            fprintf(stderr, "Cannot open file for writing: %s\n", output);
            exit(2);
        }
    }

    for (int i = 0; i < forms->count; i++) {
        value *val = forms->vals[i];
        format_value(val, out);
        for (int j = 0; j < val->new_lines; j++) {
            fputc('\n', out);
            ctx->offset = 0;
        }
        if (!val->new_lines && i < forms->count - 1 && !is_prefix(val)) {
            fputc(' ', out);
            ctx->offset++;
        }
        ctx->indent = 0;
    }

    fclose(out);
}

void format_dir(char *path) {
    char *paths[] = {path, NULL};
    FTS *ftsp = fts_open(paths, FTS_LOGICAL | FTS_NOSTAT, NULL);
    if (!ftsp) {
        fprintf(stderr, "Cannot open directory: %s\n", path);
        exit(2);
    }
    FTSENT *ent;
    while ((ent = fts_read(ftsp))) {
        switch (ent->fts_info) {
        case FTS_D:
            if (ent->fts_name[0] == '.') {
                fts_set(ftsp, ent, FTS_SKIP);
            }
            break;
        case FTS_NSOK: // Means regular file since FTS_NOSTAT was passed to
                       // fts_open.
            if (is_clj(ent->fts_name)) {
                format_file(ent->fts_accpath, ent->fts_accpath);
            }
            break;
        default:
            break;
        }
    }
    fts_close(ftsp);
}

int main(int argc, char **argv) {
    options opts;
    parse_args(argc, argv, &opts);
    pump_tries();

    DIR *d;
    if (opts.input && (d = opendir(opts.input))) {
        closedir(d);
        format_dir(opts.input);
    } else {
        format_file(opts.input, opts.output);
    }
}
