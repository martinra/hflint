// todo: when GHCi supports capi for #define statements, then remove these wrappers

#ifndef __FLINT_DEFINE_WRAPPERS_H
#define __FLINT_DEFINE_WRAPPERS_H

#include <flint/flint.h>
#include <flint/fmpq.h>
#include <flint/fmpq_mat.h>
#include <flint/fmpz_poly.h>
#include <flint/fmpq_poly.h>

// todo: when GHCi supports capi for #define statements, then remove these wrappers

fmpz* fmpq_numref_wrapper(fmpq_t x);
fmpz* fmpq_denref_wrapper(fmpq_t x);

slong fmpq_mat_nrows_wrapper(const fmpq_mat_t m);
slong fmpq_mat_ncols_wrapper(const fmpq_mat_t m);
fmpq* fmpq_mat_entryref_wrapper(const slong i, const slong j, const fmpq_mat_t m);

int fmpz_poly_is_zero_wrapper(const fmpz_poly_t p);

fmpz* fmpq_poly_denref_wrapper(fmpq_poly_t x);

ulong nmod_add_wrapper(ulong a, ulong b, nmod_t* mod);
ulong nmod_sub_wrapper(ulong a, ulong b, nmod_t* mod);
ulong nmod_neg_wrapper(ulong a, nmod_t* mod);
ulong nmod_mul_wrapper(ulong a, ulong b, nmod_t* mod);
ulong nmod_inv_wrapper(ulong a, nmod_t* mod);
ulong nmod_div_wrapper(ulong a, ulong b, nmod_t* mod);

#endif
