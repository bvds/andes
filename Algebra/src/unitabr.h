// unitabr.h	class unitabrs for tables of names of combinations of units
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
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
  unitabrs() { }
  void fill();
  string match(dimens);
  //   int size();
  numvalexp * unitget(const string & unitname);
};
