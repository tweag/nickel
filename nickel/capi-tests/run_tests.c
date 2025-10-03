#include <assert.h>
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

const char* BAD_EXAMPLE = " \
  { \
    foo | String = 1, \
  } \
";

const char* TRACE_EXAMPLE = "std.trace \"hi\" 1";

struct buffer {
  char *ptr;
  uintptr_t capacity;
  uintptr_t len;
};

struct buffer new_buffer() {
  struct buffer buf = {
    malloc(128),
    128,
    0,
  };
  return buf;
}

uintptr_t write_callback(void *context, uint8_t const* buf, uintptr_t len) {
  struct buffer *out_buf = (struct buffer*) context;
  while (out_buf->capacity <= out_buf->len + len) {
    out_buf->capacity *= 2;
    out_buf->ptr = realloc(out_buf->ptr, out_buf->capacity);
    assert(out_buf->ptr != NULL);
  }

  memcpy(out_buf->ptr + out_buf->len, buf, len);
  out_buf->len += len;
  out_buf->ptr[out_buf->len] = '\0';

  return len;
}

int main() {
  nickel_context *ctx = nickel_context_alloc();
  nickel_expr *expr = nickel_expr_alloc();
  nickel_error *error = nickel_error_alloc();

  nickel_result result = nickel_context_eval_deep(ctx, EXAMPLE, expr, error);
  assert(result == NICKEL_RESULT_OK);

  assert(nickel_expr_is_record(expr));
  nickel_record const *rec = nickel_expr_as_record(expr);
  assert(nickel_record_len(rec) == 1);

  char const* key;
  uintptr_t len;
  nickel_expr *val = nickel_expr_alloc();
  nickel_record_key_value_by_index(rec, 0, &key, &len, val);

  assert(len == 3);
  assert(!strncmp(key, "foo", 3));
  assert(nickel_expr_is_number(val));

  // Test error reporting
  result = nickel_context_eval_deep(ctx, BAD_EXAMPLE, expr, error);
  assert(result == NICKEL_RESULT_ERR);

  struct buffer buf = new_buffer();
  nickel_error_display(error, write_callback, &buf, NICKEL_ERROR_FORMAT_ANSI_TEXT);
  fputs(buf.ptr, stderr);
  assert(strstr(buf.ptr, "contract broken by the value") != NULL);

  // Test tracing
  buf.len = 0;
  nickel_context_set_trace_callback(ctx, write_callback, NULL, &buf);
  result = nickel_context_eval_deep(ctx, TRACE_EXAMPLE, expr, error);
  assert(result == NICKEL_RESULT_OK);
  assert(!strcmp(buf.ptr, "std.trace: hi\n"));

  nickel_expr_free(val);
  nickel_expr_free(expr);
  nickel_error_free(error);
  nickel_context_free(ctx);
}
