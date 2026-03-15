#include <R_ext/Rdynload.h>

void R_init_parallax_extendr(DllInfo *dll);

void R_init_parallax(DllInfo *dll) {
  R_init_parallax_extendr(dll);
}
