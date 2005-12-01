//  lookslikeint
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// changed fuzziness from 1.E-8 to RELERR, 4/23/01 No change in 100 prob sols.
#include <math.h>
#include "extstruct.h"

// no diagnostics
// Previously, there was no special handling for numbers near zero,
// but there are several places where it is assumed that the error
// is relative to 1.

bool lookslikeint(double v, int &q) // if double v is close to an integer,
{				// return true; place nearest integer in q
  
  // cast from float to integer truncates at decimal point
  q = (v>-0.5 ? v+0.5 : v-0.5); 
  // assume relevant error scale is one.
  return (fabs(v-q) <= RELERR 
	  // floating point representation necessitates relative error:
	  || fabs(v-q) <= fabs(v)*RELERR);
}
