// donlsolv simply applies nlsolv to each equation in list of eqs
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

/************************************************************************
 * bool donlsolv  applies nlsolvov to each equation in eqn, 		*
 * returns true if nlsolvov did so on any of them			*
 ************************************************************************/
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include "binopfunctions.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)

bool donlsolv(vector<binopexp *> * eqn)
{
  bool answer = false;
  for (int k = 0; k < eqn->size(); k++)
    {
      if (nlsolvov((*eqn)[k])) {
	answer = true;
	DBG( cout << "nlsolvov succeeded in finding " << (*eqn)[k]->getInfix()
	          << endl << "Forcing repeat" << endl;);
      }
      else NEWDTL( cout << "nlsolvov failed to solve " << (*eqn)[k]->getInfix()
			<< endl; );
    }
  return(answer);
}
