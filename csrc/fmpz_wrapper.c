#include "fmpz_wrapper.h"

void
fmpz_init_wrapper(
  fmpz_t f
)
{
  fmpz_init(f);
}

void
fmpz_clear_wrapper(
  fmpz_t f
)
{
  fmpz_clear(f);
}

void
fmpz_set_si_wrapper(
  fmpz_t f,
  slong val
)
{
  fmpz_set_si(f, val);
}

void
fmpz_neg_wrapper(
  fmpz_t f1,
  const fmpz_t f2
)
{
  fmpz_neg(f1, f2);
}

