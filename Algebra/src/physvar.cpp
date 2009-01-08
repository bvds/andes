// Class physvar, march 2001
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

#include <string>
#include "decl.h"
#include <stdio.h>
// ?    #include <math.h>
using namespace std;
#include "dbg.h"

#define DBG(A) DBGF(EXPRDB,A)

physvar::~physvar() {
}

void physvar::putclipsname(string newname)
{
  this->clipsname.assign(newname);
}

void physvar::putshortname(string newname)
{
  this->shortname.assign(newname);
}

void physvar::putvartype(vartype type)
{
  this->type = type;
}

void physvar::putdimens(int lengthd, int massd, int timed, int charged,
			 int tempd)
{
 MKS.put(lengthd, massd, timed, charged, tempd);
}

void physvar::putdimens(double lengthd, double massd, double timed, 
			double charged, double tempd)
{
  MKS.put(lengthd, massd, timed, charged, tempd);
}

