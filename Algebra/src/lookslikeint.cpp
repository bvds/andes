//  lookslikeint
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// changed fuzziness from 1.E-8 to RELERR, 4/23/01 No change in 100 prob sols.
#include <math.h>
#include "extstruct.h"

// no diagnostics
// Previously, there was special handling for numbers near zero,
// but this is inconsistant with what one would expect.

bool lookslikeint(double v, int &q) // if double v is close to an integer,
{				// return true and place integer in q
  if (floor(v + RELERR) > floor(v - RELERR) + 0.5)
    {
      if(fabs(v)<RELERR && v!=0){
	cout << "lookslikeint case incorrect previously" <<endl;
      }

      // cast from float to integer truncates at decimal point
      q = (v>-0.5 ? v+0.5 : v-0.5); 
      return(true);
    }
  else return(false);
}
