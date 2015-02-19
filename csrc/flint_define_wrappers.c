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

slong
fmpq_mat_nrows_wrapper(
  const fmpq_mat_t m
  )
{
  return fmpq_mat_nrows(m);
}

slong
mpq_mat_ncols_wrapper(
  const fmpq_mat_t m
  )
{
  return fmpq_mat_ncols(m);
}

fmpq*
fmpq_mat_entryref_wrapper(
  const slong i,
  const slong j,
  const fmpq_mat_t m
  )
{
  return fmpq_mat_entry(m, i,j);
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
