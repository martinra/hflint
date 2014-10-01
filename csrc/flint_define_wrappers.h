// todo: when GHCi supports capi for #define statements, then remove these wrappers

#ifndef __FLINT_DEFINE_WRAPPERS_H
#define __FLINT_DEFINE_WRAPPERS_H

#include <flint/flint.h>
#include <flint/fmpq.h>
#include <flint/fmpq_mat.h>

// todo: when GHCi supports capi for #define statements, then remove these wrappers

fmpz* fmpq_numref_wrapper(fmpq_t x);
fmpz* fmpq_denref_wrapper(fmpq_t x);

slong fmpq_mat_nrows_wrapper(const fmpq_mat_t m);
slong fmpq_mat_ncols_wrapper(const fmpq_mat_t m);
fmpq* fmpq_mat_entryref_wrapper(const slong i, const slong j, const fmpq_mat_t m);

#endif
