#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "nickel_lang.h"

const char* EXAMPLE = " \
  { \
    foo = 1, \
  } \
";

int main() {
  Context *ctx = nickel_context_new();
  Expr *expr = nickel_expr_alloc();
  Error *error = NULL;
  nickel_context_eval_deep(ctx, EXAMPLE, expr, &error);

  printf("%p %p\n", expr, error);

  printf("is record: %d\n", nickel_expr_is_record(expr));
  Record const *rec = nickel_expr_as_record(expr);
  printf("record len: %ld\n", nickel_record_len(rec));

  char const* key;
  uintptr_t len;
  Expr *val = nickel_expr_alloc();
  nickel_record_key_value_by_index(rec, 0, &key, &len, val);

  char *key_str = malloc(len + 1);
  memcpy(key_str, key, len);
  key_str[len] = '\0';

  printf("key %s, value number: %d\n", key_str, nickel_expr_is_number(val));

  nickel_expr_free(val);
  nickel_expr_free(expr);
  nickel_context_free(ctx);
}
