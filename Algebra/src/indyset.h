// indyset.h	a class for independent sets of equations as functions
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

#include "expr.h"
#include "valander.h"

/************************************************************************
 * Maintains a list of independent functions (representing eqn->lhs = 0)*
 *	 in the form of their gradients at the solution point.	 	*
 *    The basis vectors are generated sequentially from the presented	*
 *	gradients by projecting out previous equations so as to 	*
 *	keep the matrix a column permutation of upper triangular, with	*
 *	the order kept in ordervar (viz.)				*
 ************************************************************************/

class indyset
{
private:
  vector<vector<double> > basis;   // basis vectors for gradients already in
				// the set of independent equations.
  int numvars;
  int numinset;
  vector<int> ordervar;		// order of variables to eliminate. 
				// basis[i][ordervar[j]] is 0 for i > j
  vector<vector<double> > basexpand;	// basis[i] is basexpand[i][j]
				// *grad expr[j], where expr[j] is 
				// the j'th expression presented.
  bool lastisvalid;		// set when candidate is expanded, to enable
				// placelast or expandlast
  vector<double> candexpand;	// expansion of candidate gradient in terms
				// of basis (not expr - this is done on req)
  vector<double> candleft;	// gradient of cand minus stuff from candexpand
  vector<double> candleft_err;	// associated error
public:
  indyset(int numvars);
  bool isindy(const expr * const candex);
  bool isindy(const valander * const candval);
  bool placelast();
  vector<double> *expandlast();
  bool keepn(int n);
  ~indyset() { keepn(0); }
  int size() { return(numinset); }
};
