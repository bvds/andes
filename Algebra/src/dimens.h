// dimens.h		physical dimensions for a physics quantity
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#ifndef DIMENSH
#define DIMENSH
#include <string>
using namespace std;

#define MULTP 12
#define MAXP 10
#define UNKNDIM -127
#define INCONS -126
#define MAYBZ 125
#define OVERFL 121
typedef signed char DIMEXP;	// dimensions are stored as
		// signed DIMEXPs of MULTP times the real
		// dimension, and have max abs value MAXP
// special values are +-OVERFL for overflows, UNKNDIM if dimension
//  is unknown at present, INCONS (for expressions) if there is an
//  incopnsistancy, and possibly MAYBZ if probably but not certainly zero
class dimens
{
 private:
  DIMEXP dims[5];
    //  lengthdim,  massdim,  timedim, chargedim, tempdim, in that order;
 public:
  dimens();
  dimens(int,int,int,int,int);
  dimens(double,double,double,double,double);
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
  bool incons();
  dimens& operator+=(const dimens a);
  dimens& operator*=(const double);
  bool adjust(const dimens & a);
  bool operator==(const dimens b) const;
  dimens operator*(const double k) const;
  dimens operator+(const dimens b) const;

  friend class physvar;
  friend class expr;
  friend class binopexp;
  friend class functexp;
  friend class n_opexp;
};

#endif
