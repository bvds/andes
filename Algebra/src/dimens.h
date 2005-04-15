// dimens.h		physical dimensions for a physics quantity
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

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
  DIMEXP addem(const int a, const int b);
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
  bool zerop();
  bool unknp();
  bool inconsp();
  dimens& operator+=(const dimens a);
  dimens& operator*=(const double);
  bool adjust(const dimens & a);
  bool operator==(const dimens b) const;
  dimens operator*(const double k) const;
  dimens operator+(const dimens b) const;
};

#endif
