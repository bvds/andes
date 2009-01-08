//  lookslikeint
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
// Modifications by Brett van de Sande, 2005-2008
//
//  This file is part of the Andes Solver.
//
//  The Andes Solver is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  The Andes Solver is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public License
//  along with the Andes Solver.  If not, see <http://www.gnu.org/licenses/>.

// changed fuzziness from 1.E-8 to RELERR, 4/23/01 No change in 100 prob sols.
#include <math.h>
#include "extstruct.h"

// no diagnostics
// Previously, there was no special handling for numbers near zero,
// but there are several places where it is assumed that the error
// is relative to 1.

bool lookslikeint(double v, int &q) // if double v is close to an integer,
{				// return true; place nearest integer in q
  //  cout << "lookslikeint for v=" << v << endl;
  if(fabs(v)>(double) INT_MAX) return(false); // too big to be an integer
  // cast from float to integer truncates at decimal point
  q = (v>-0.5 ? v+0.5 : v-0.5); 
  // assume relevant error scale is one.
  return (fabs(v-q) <= RELERR 
	  // floating point representation necessitates relative error:
	  || fabs(v-q) <= fabs(v)*RELERR);
}
