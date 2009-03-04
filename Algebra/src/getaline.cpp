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
