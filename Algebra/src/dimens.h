// dimens.h		physical dimensions for a physics quantity
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

#ifndef DIMENSH
#define DIMENSH
#include <string>
using namespace std;

typedef signed char DIMEXP;	// dimensions are stored as
		// signed DIMEXPs of MULTP times the real
		// dimension, and have max abs value MAXP
// special values are +-OVERFL for overflows, UNKNDIM if dimension
//  is unknown at present, INCONS (for expressions) if there is an
//  incopnsistancy, and possibly MAYBZ if probably but not certainly zero
class dimens
{
 private:
  //  lengthdim,  massdim,  timedim, chargedim, tempdim, in that order;
  DIMEXP dims[5];
  static const DIMEXP UNKNDIM;
  static const DIMEXP INCONS;
  static const DIMEXP MULTP;
  static const DIMEXP MAXP;
  static const DIMEXP MAYBZ;
  static const DIMEXP OVERFL;
  DIMEXP addem(const int a, const int b) const;
 public:
  dimens();
  dimens(int,int,int,int,int);
  dimens(double,double,double,double,double);
  void set_incons();  // make into an inconsistant
  void set_unkn();  // make into an unknown
  void put(int lengthd, int massd, int timed);
  void put(int lengthd, int massd, int timed, int charged, int tempd);
  void put(double lengthd, double massd, double timed);
  void put(double lengthd, double massd, double timed, double charged, 
	   double tempd);
  double getmassd() const;
  double getlengthd() const;
  double gettimed() const;
  double getcharged() const;
  double gettempd() const;
  string print();
  bool zerop() const;
  bool unknp() const;
  bool inconsp() const;
  dimens& operator+=(const dimens a);
  dimens& operator*=(const double);
  bool adjust(const dimens & a);
  bool operator==(const dimens b) const;
  dimens operator*(const double k) const;
  dimens operator+(const dimens b) const;
};

#endif
