// todo: when GHCi supports capi for #define statements, then remove these wrappers

#ifndef __FLINT_DEFINE_WRAPPERS_H
#define __FLINT_DEFINE_WRAPPERS_H

#include <flint/flint.h>
#include <flint/fmpq.h>

// todo: when GHCi supports capi for #define statements, then remove these wrappers

fmpz* fmpq_numref_wrapper(fmpq_t x);
fmpz* fmpq_denref_wrapper(fmpq_t x);

#endif
