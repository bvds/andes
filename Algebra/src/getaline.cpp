// getaline.cc   
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

// my version of getline - returns a string of arbitrary 
// length by reading one line. Drops last character if it is a ^M or ^J
//
// string getaline(istream instr)

#include <iostream>
#include <string>
using namespace std;

string getaline(istream& in) {
	std::string str("");

	while (in) {
		char c = in.get();
		if (in) {
			if ((c == 0x0d) || (c == 0x0a) || (c < 0)) {
				break;
			}
			str += c;
		}
	}
	return str;
}

// no diagnostics

string geta2line(istream &instr)
{
  char buf[513];
  string bufst = string("");
  int k, j;
  char linebr;

  //  cerr << "entered getaline " << endl; // diag
  bool endfound = false;
  instr.get(buf,513);
  //  cerr << "got buf " << bufst << endl; // diag
  if (instr.bad()) throw(string("fatal error in reading line in getaline\n"));
  k=instr.gcount();
  //  cerr << "k = " << k  << endl; // diag
  while ((k==512) && (instr.peek()!=10) && (instr.peek()!=13))
  {
    //    cerr << "entered while in getaline " << endl; // diag
    bufst.append(buf,k);
    instr.get(buf,513);
    if (instr.bad())throw(string("fatal error in reading line in getaline\n"));
    k=instr.gcount();
    //    cerr << "k = " << k  << endl; // diag
  }
  if (k!=0) if ((buf[k-1]=='\012')||(buf[k-1]=='\015'))
    k -= 1;

  //  cerr << "about to append up to " << k << endl; // diag
  bufst.append(buf,0,k);
  //  cerr << "bufst is now "<< bufst<<" of length "  // diag
  // << bufst.size() << endl; // diag
  instr.get(linebr);	// eat linebreak
  if((!instr.eof()) && (cin != instr))
    {
      j = instr.peek();	// two unequal ^M and ^J are one linebreak
      //      cerr << "k = " << k  << endl; // diag
      if (((j==10)||(j==13)) && (j!= (int) linebr)) instr.get(linebr);
    }
  //  cerr << "about to return from getaline "<< bufst  << endl; // diag
  return(bufst);
}
