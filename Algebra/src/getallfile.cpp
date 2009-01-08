// getallfile.cpp
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

// read in equations and variable specs from Collin/Anders style file,
//   and feed them, a line at a time, to getall

#include <fstream>
#include <string>
using namespace std;

bool getall(string bufst);
string getaline(istream &instr);

int numparams;
bool getallfile(istream & infile )
{
  string bufst;
  numparams = 0;
  while (!infile.eof())
    { 
      bufst = getaline(infile);
      if (bufst.empty()) continue;
      if (!getall(bufst)) return(false);
    }
  return(true);
}
