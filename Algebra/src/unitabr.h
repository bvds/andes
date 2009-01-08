// unitabr.h	class unitabrs for tables of names of combinations of units
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
//  declares class unitabrs  which now (3/22/01) handles prefixes & MKS
#include <string>
#include <vector>
#include "expr.h"
#include "dimens.h"

using namespace std;

class unitabrs
{
  vector<string> abbrev;
  vector<double> value;
  vector<dimens> dims;
  vector<bool> pfxable;
  vector<string> pfxs;
  vector<double> pfxvals;
public:
  unitabrs(void); 
  string match(dimens);
  //   int size();
  numvalexp * unitget(const string & unitname);
};
