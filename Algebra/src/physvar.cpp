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

double physvar::getlengthd() const { return  (MKS.dims[0] * 1.0 / MULTP); }
double physvar::getmassd() const { return  (MKS.dims[1] * 1.0 / MULTP); }
double physvar::gettimed() const { return  (MKS.dims[2] * 1.0 / MULTP); }
double physvar::getcharged() const { return  (MKS.dims[3] * 1.0 / MULTP); }
double physvar::gettempd() const { return  (MKS.dims[4] * 1.0 / MULTP); }
