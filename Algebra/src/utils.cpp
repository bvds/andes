//  strutils.cpp   current utils includes
//	
//	string dtostr(double val);
//	string itostr(int val);
//  
#include <string>
#include <vector>
#include <iostream>
#include <stdio.h>
using namespace std;

string dtostr(double val)
{
  char buf[14];
  sprintf(buf,"%13lg",val);
  return(string(buf));
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
      cout << vec[k];
      if (k+1 != vec.size()) cout << ", ";
      else cout << endl;
    }
}
