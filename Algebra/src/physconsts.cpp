// physconsts.cpp	get known constants from file
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include <vector>
#include "dimens.h"
#include "expr.h"
#include <fstream>
#include "dbg.h"
using namespace std;
#pragma warning (disable: 4786)
#define Asize(arr) (sizeof(arr)/sizeof(arr[0]))

#define DBG(A) DBGF(UNITS,A)

extern vector<string> * constnames;
extern vector<numvalexp *> * constnumvals;

struct physc {
  string name;
  double value;
  int lenu;
  int massu;
  int secu;
  int chgu;
  int ku;
	physc() {}
	physc(char* n, double v, int l, int m, int s, int c, int k) {
		name = n; value = v; lenu = l; massu = m; secu = s; chgu = c; ku = k;
	}
} pctab[] = {
#include "pconsts.h"
};

void constsfill()
{
  string name;
  dimens dim;
  constnames = new vector<string>;
  constnumvals = new vector<numvalexp *>;
  numvalexp * thisnumval;
  for (int k = 0; k < Asize(pctab); k++) {
    constnames->push_back(pctab[k].name);
    thisnumval = new numvalexp(pctab[k].value);
    thisnumval->MKS.put(pctab[k].lenu, pctab[k].massu, pctab[k].secu,
	    pctab[k].chgu, pctab[k].ku);
    constnumvals->push_back(thisnumval);
  }
}		 
