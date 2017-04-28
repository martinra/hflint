#include "flint_define_wrappers.h"

fmpz*
fmpq_numref_wrapper(
  fmpq_t x
  )
{
  return fmpq_numref(x);
}

fmpz*
fmpq_denref_wrapper(
  fmpq_t x
  )
{
  return fmpq_denref(x);
}

fmpq*
fmpq_mat_entry_wrapper(
  const fmpq_mat_t m,
  const slong i,
  const slong j
  )
{
  return fmpq_mat_entry(m, i,j);
}

slong
fmpq_mat_nrows_wrapper(
  const fmpq_mat_t m
  )
{
  return fmpq_mat_nrows(m);
}

slong
fmpq_mat_ncols_wrapper(
  const fmpq_mat_t m
  )
{
  return fmpq_mat_ncols(m);
}

fmpz*
fmpq_poly_denref_wrapper(
  fmpq_poly_t x
  )
{
  return fmpq_poly_denref(x);
}

int
fmpz_poly_is_zero_wrapper(
  const fmpz_poly_t p
  )
{
  return fmpz_poly_is_zero(p);
}

ulong
nmod_add_wrapper(
  ulong a,
  ulong b,
  nmod_t* mod
  )
{
  return nmod_add(a,b,*mod);
}

ulong
nmod_sub_wrapper(
  ulong a,
  ulong b,
  nmod_t* mod
  )
{
  return nmod_sub(a,b,*mod);
}

ulong
nmod_neg_wrapper(
  ulong a,
  nmod_t* mod
  )
{
  return nmod_neg(a,*mod);
}

ulong
nmod_mul_wrapper(
  ulong a,
  ulong b,
  nmod_t* mod
  )
{
  return nmod_mul(a,b,*mod);
}

ulong
nmod_inv_wrapper(
  ulong a,
  nmod_t* mod
  )
{
  return nmod_inv(a,*mod);
}

ulong
nmod_div_wrapper(
  ulong a,
  ulong b,
  nmod_t* mod
  )
{
  return nmod_div(a,b,*mod);
}

void
nf_elem_clear_wrapper(
  const nf_t nf,
  nf_elem_t a
  )
{
  nf_elem_clear(a, nf);
}
