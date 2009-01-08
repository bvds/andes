// valander.h
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
 * class valander contains the value and the gradient of a function	*
 *	at the solution point.						*
 *   new version also includes a vector of bools, hasvar[k] telling 	*
 *	whether the function has any dependence on the variable k	*
 ************************************************************************/
#ifndef VALANDEF
#define VALANDEF
#include <vector>
class valander
{
public:
  double value;
  vector<double> gradient;
  vector<bool> hasvar;
  valander(int numvars) {
    gradient.assign(numvars,0.0);
    hasvar.assign(numvars,false);
  }
  string print();
};

valander * getvnd(const expr * ex, const vector<physvar *> * vars,
                  const vector<double> * sols); // in valander.cpp
#endif

