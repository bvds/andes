// getallfile.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// read in equations and variable specs from Collin/Anders style file,
//   and feed them, a line at a time, to getall

#include <fstream>
#include <string>
using namespace std;

bool getall(string bufst);
string getaline(istream &instr);

int numparams;
bool getallfile(ifstream & infile )
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
