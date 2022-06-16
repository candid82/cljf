#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *bytes;
    size_t size;
    char *sp;
} context;

typedef enum { V_STRING, V_UKNOWN } value_type;

typedef struct {
    value_type type;
    char *start;
    char *end;
} value;

char *value_type_names[V_UKNOWN] = {"string"};
context *ctx;

value *make_value(value_type type, char *start, char *end) {
    value *res = malloc(sizeof(value));
    res->type = type;
    res->start = start;
    res->end = end;
    return res;
}

void print_usage(void) {
    fprintf(stderr, "Usage: cljf <filename>\n");
    exit(1);
}

void print_value(value *val) {
    printf("%s[%ld]:%.*s\n", value_type_names[val->type],
           (val->end - val->start), (int)(val->end - val->start), val->start);
}

value *read_string(void) {
    char *start = ctx->sp;
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
    return make_value(V_STRING, start, ctx->sp);
}

void skip_whitespace(void) {
    // Note: doesn't handle unicode whitespaces (yet?).
    while (1) {
        switch (*ctx->sp) {
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
            ctx->sp++;
        default:
            return;
        }
    }
}

value *read_value(void) {
    skip_whitespace();
    if (!*ctx->sp)
        return NULL;
    switch (*ctx->sp) {
    case '"':
        return read_string();
        break;
    default:
        fprintf(stderr, "Unexpected character: %c\n", *ctx->sp);
        exit(4);
        break;
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

int main(int argc, char **argv) {
    if (argc != 2) {
        print_usage();
    }

    ctx = malloc(sizeof(context));
    read_file(argv[1], ctx);

    value *val = NULL;
    while ((val = read_value())) {
        print_value(val);
    }
}
