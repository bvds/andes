// linnfilt.h	functions defined in convert.cpp
// solver routines
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
//////////////////////////////////////////////////////////////////////////////
char * solveBubbleFile(const char* const src, const char* const dst);
char * solveBubble();
char * solveMoreBubble();
char * solveAdd(const char* const lispExpression);
char * solveClear();
// char * c_indyIsIndependent(const char* const data);
char * c_indyAddVariable(const char* const data);
char * c_indyDoneAddVariable();
char * c_indyAddEquation(const char* const data);
char * c_indyEmpty();
char * c_indyAddEq2Set(const char* const data);
char * c_indyKeepNOfSet(const char* const data);
// char * c_indyExpandInSet(const char* const data);
// char * c_indyStudentExpandInSet(const char* const data);
// char * c_indyStudentIsIndependent(const char* const data);
char * c_indyStudHowIndy(const char* const data);
char * c_indyCanonHowIndy(const char* const data);
char * c_indyStudentAddEquationOkay(const char* const data);
