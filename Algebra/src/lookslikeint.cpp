//  lookslikeint
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// changed fuzziness from 1.E-8 to RELERR, 4/23/01 No change in 100 prob sols.
#include <math.h>
#include "extstruct.h"

// no diagnostics

bool lookslikeint(double v, int &q) // if double v is close to an integer,
{				// return true and place integer in q
  // AW: test below doesn't work for non-zero values smaller than RELERR. 
  if ((fabs(v) <= RELERR) && (v != 0)) return(false);

  if (floor(v + RELERR) - floor(v - RELERR) > .5)
    {
      q = (int) floor(v + RELERR);
      return(true);
    }
  else return(false);
}
