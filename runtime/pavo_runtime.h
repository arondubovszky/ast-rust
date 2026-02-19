#ifndef PAVO_RUNTIME_H
#define PAVO_RUNTIME_H

#include <stdint.h>

void pavo_println_i32(int32_t x);
void pavo_println_i64(int64_t x);
void pavo_println_f32(float x);
void pavo_println_f64(double x);
void pavo_println_bool(int32_t x);
void pavo_println_str(const char* s);

#endif
