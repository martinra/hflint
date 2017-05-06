#ifndef __FLINT_DEFINE_ADDITIONALS_H
#define __FLINT_DEFINE_ADDITIONALS_H

#include <flint/flint.h>
#include <flint/fmpz.h>
#include <flint/fmpz_poly.h>
#include <flint/nf.h>
#include <flint/nmod_vec.h>
#include <gmp.h>

// todo: when flint has addopted these functions use them

void fmpz_poly_factor_get_content_additional(fmpz_t c, const fmpz_poly_factor_t fac);
slong fmpz_poly_factor_number_factors_additional(const fmpz_poly_factor_t fac);
slong fmpz_poly_factor_get_factor_additional(fmpz_poly_t p, const fmpz_poly_factor_t fac, slong i);

mp_limb_t nmod_n_additional(nmod_t * mod);

#endif
