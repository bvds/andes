//  strutils.cpp   current utils includes
//	
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
  sprintf(buf,"%12d",val);
  return(string(buf));
}

void printdv(const vector<double> & vec)
{
  for (int k=0; k < vec.size(); k++)
    {
      if(vec[k]==0.) continue;
      if (k>0) cout << ", ";
      cout << "(" << k << "," << vec[k] << ")";
    }
  cout << endl;
}
