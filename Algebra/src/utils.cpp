//  strutils.cpp   current utils includes
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

//	string dtostr(double val);
//	string itostr(int val);
//  
#include "decl.h"
#include <string>
#include <vector>
#include <iostream>
#include <stdio.h>
using namespace std;

// This is a copy of multiple copies of code in exprp.cpp
string dtostr(double value)
{
  int q;
  char valuenum[30];
  // The number of digits are supposed to match DBL_EPSILON
  // don't truncate nonzero numbers near zero
  if ((value==0. || fabs(value)>0.5) && lookslikeint(value,q))
    sprintf(valuenum,"%d",q);
  else if ((fabs(value) < 1.) && (fabs(value)> 0.001))
    sprintf(valuenum,"%.17lf",value);
  else
    sprintf(valuenum,"%.17lG",value);
  return(string(valuenum));
}

string itostr(int val)
{
  char buf[13];
  sprintf(buf,"%d",val);
  return(string(buf));
}

void printdv(const vector<double> & vec)
{
  bool flag=false;
  for (int k=0; k < vec.size(); k++)
    {
      if(vec[k]==0.) continue;
      if (flag) cout << ", ";
      cout << "(" << k << "," << vec[k] << ")";
      flag=true;
    }
  cout << " [" << vec.size() << "]";
}
