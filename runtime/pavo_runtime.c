#include "pavo_runtime.h"
#include <stdio.h>

void pavo_println_i32(int32_t x)  { printf("%d\n",  x); }
void pavo_println_i64(int64_t x)  { printf("%lld\n", x); }
void pavo_println_f32(float x)    { printf("%f\n",  x); }
void pavo_println_f64(double x)   { printf("%f\n",  x); }
void pavo_println_bool(int32_t x) { printf("%s\n",  x ? "true" : "false"); }
void pavo_println_str(const char* s) { printf("%s\n", s); }
