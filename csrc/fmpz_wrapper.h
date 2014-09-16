#ifndef __FMPZ_WRAPPER_H
#define __FMPZ_WRAPPER_H

#include "flint/fmpz.h"

void fmpz_init_wrapper(fmpz_t f);

void fmpz_clear_wrapper(fmpz_t f);

void fmpz_set_si_wrapper(fmpz_t f, slong val);

void fmpz_neg_wrapper(fmpz_t f1, const fmpz_t f2);

#endif
