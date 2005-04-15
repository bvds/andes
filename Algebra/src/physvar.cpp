// Class physvar, march 2001
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

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

