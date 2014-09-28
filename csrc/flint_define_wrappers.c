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
