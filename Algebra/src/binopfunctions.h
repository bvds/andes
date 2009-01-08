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

#ifndef BINOPFUNCTIONS_INCLUDED
#define BINOPFUNCTIONS_INCLUDED
#include "expr.h"
#include "decl.h"
//
//  Associated function prototypes
// 

void recassign( vector<binopexp *> * eqn, vector<varindx> * & vars,
		vector<binopexp *> * soleqs);		       // recassign.cpp
void dopurelin(vector<binopexp *> *eqn, 	// purelin.cpp
			       vector<varindx> * & vars, 
			       vector<binopexp *> * soleqs,
			       vector<binopexp *> *partsols,
			       int & doagain );
bool dofactor(vector<binopexp *> * eqn, vector<varindx> * & vars);
bool donlsolv(vector<binopexp *> * eqn);			// donlsolv.cpp
int dotrig(vector<binopexp *> * eqn);			 // dotrig.cpp
#endif
