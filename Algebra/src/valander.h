// valander.h
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

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

