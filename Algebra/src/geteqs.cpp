// geteqs.cc defined geteqsfromeqsfile
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// vector<string> geteqsfromeqsfile(istream)
//
// Note - version for eqs.clp - assumes each equation on separate single line

#include <fstream>
#include <string>
#include <vector>
using namespace std;

// no diagnostics
string getaline(istream &instr);				// getaline.cpp

vector<string> *geteqsfromeqsfile(istream & infile )
{
  vector<string> *eqs = new vector<string>;
  string bufst;

  while (!infile.eof())
    { 
      bufst = getaline(infile);
      if ((!bufst.empty())&&(bufst[0]=='('))
	eqs->push_back(bufst); // this assumes equations have
				// been reformated 1 per line, and ends
				// with something that doesn't begin with (
    }
  return(eqs);
}
