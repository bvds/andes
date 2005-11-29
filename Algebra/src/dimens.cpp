// dimens.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "dimens.h"
#include "decl.h"
#include <string>
#include <stdio.h>
using namespace std;

// no diagnostics

const DIMEXP dimens::UNKNDIM=-127;
const DIMEXP dimens::INCONS=-126;
const DIMEXP dimens::MULTP=12;
const DIMEXP dimens::MAXP=10;
const DIMEXP dimens::MAYBZ=125;
const DIMEXP dimens::OVERFL=121;


dimens::dimens() { set_unkn(); } // constrictor sets to unknown

dimens::dimens(int lengthd, int massd, int timed, int charged, int tempd)
{ 
  dims[0] = (DIMEXP)  ( MULTP * lengthd );
  dims[1] = (DIMEXP)  ( MULTP * massd );
  dims[2] = (DIMEXP)  ( MULTP * timed );
  dims[3] = (DIMEXP)  ( MULTP * charged );
  dims[4] = (DIMEXP)  ( MULTP * tempd );
}

dimens::dimens(double lengthd, double massd, double timed, double charged, 
	   double tempd)
{ 
  dims[0] = (DIMEXP)  ( MULTP * lengthd );
  dims[1] = (DIMEXP)  ( MULTP * massd );
  dims[2] = (DIMEXP)  ( MULTP * timed );
  dims[3] = (DIMEXP)  ( MULTP * charged );
  dims[4] = (DIMEXP)  ( MULTP * tempd );
}

void dimens::set_incons(){
  for (int k = 0; k < 5; k++) dims[k] = INCONS;}

void dimens::set_unkn(){
  for (int k = 0; k < 5; k++) dims[k] = UNKNDIM;}

void dimens::put(int lengthd, int massd, int timed, int charged, int tempd)
{ 
  dims[0] = (DIMEXP)  ( MULTP * lengthd );
  dims[1] = (DIMEXP)  ( MULTP * massd );
  dims[2] = (DIMEXP)  ( MULTP * timed );
  dims[3] = (DIMEXP)  ( MULTP * charged );
  dims[4] = (DIMEXP)  ( MULTP * tempd );
}

void dimens::put(double lengthd, double massd, double timed, double charged, 
	   double tempd)
{ 
  dims[0] = (DIMEXP)  ( MULTP * lengthd );
  dims[1] = (DIMEXP)  ( MULTP * massd );
  dims[2] = (DIMEXP)  ( MULTP * timed );
  dims[3] = (DIMEXP)  ( MULTP * charged );
  dims[4] = (DIMEXP)  ( MULTP * tempd );
}

double dimens::getlengthd() const { return  (dims[0] * 1.0 / MULTP); }
double dimens::getmassd()   const { return  (dims[1] * 1.0 / MULTP); }
double dimens::gettimed()   const { return  (dims[2] * 1.0 / MULTP); }
double dimens::getcharged() const { return  (dims[3] * 1.0 / MULTP); }
double dimens::gettempd()   const { return  (dims[4] * 1.0 / MULTP); }

bool dimens::unknp() const	// are all dims unknown? Is that what we want?
{				//  prob better are any dims unknown!
  for (int k = 0; k < 5; k++) if (dims[k] !=  UNKNDIM) return(false);
  return(true);
}

bool dimens::inconsp() const	// are dimens inconsistent. Keep all the 
{				//  same, but say yes if any is
  for (int k = 0; k < 5; k++) if (dims[k] ==  INCONS) return(true);
  return(false);
}

bool dimens::zerop() const	// is this quantity dimensionless
{
  for (int k = 0; k < 5; k++) if (dims[k] !=  0) return(false);
  return(true);
}

/************************************************************************
 * dimens::adjust(dim) If "this" dimens is UNKNDIM or MAYBZ, set it to	*
 *	dim, except don't change MAYBZ to UNKNDIM			*
 *    returns true if "this" is modified				*
 ************************************************************************/
bool dimens::adjust(const dimens & a)
{
  bool ret = false;
  for (int k = 0; k < 5; k++)
    {
       if ((dims[k] == UNKNDIM) && (a.dims[k] != UNKNDIM))
	{
	  dims[k] = a.dims[k];
	  ret = true;
	}
      if ((dims[k] == MAYBZ) && ( a.dims[k] != UNKNDIM) &&
	  ( a.dims[k] != MAYBZ))
	{
	  dims[k] = a.dims[k];
	  ret = true;
	}
    }
  return(ret);
}

bool dimens::operator==(const dimens b) const
{ 
  for (int k = 0; k < 5; k++) if (dims[k] !=  b.dims[k]) return(false);
  return(true);
}

dimens dimens::operator*(const double km) const
{
  dimens retdim;
  int temp, k, q;
  for (k = 0; k < 5; k++)
    {
      switch (dims[k])
	{
	case UNKNDIM:
	case INCONS:
	case MAYBZ:
	  retdim.dims[k] = dims[k];
	  break;
	case OVERFL:
	case -OVERFL:
	  if (km > 0) retdim.dims[k] = dims[k];
	  else  retdim.dims[k] = -dims[k];
	  break;
	default:
	  temp = (lookslikeint(km * dims[k],q)) ? q : UNKNDIM;
	  if (temp > MULTP * MAXP) temp = OVERFL;
	  if ((temp < -MULTP * MAXP) && (temp != UNKNDIM)) temp = -OVERFL;
	  retdim.dims[k] = temp;
	}
    }
  return (retdim);
}

dimens & dimens::operator*=(const double km)
{
  int temp, k, q;
  for (k = 0; k < 5; k++)
    {
      switch (dims[k])
	{
	case UNKNDIM:
	case INCONS:
	case MAYBZ:
	  temp = dims[k];
	  break;
	case OVERFL:
	case -OVERFL:
	  if (km > 0) temp = dims[k];
	  else  temp = -dims[k];
	  break;
	default:
	  temp = (lookslikeint(km * dims[k],q)) ? q : UNKNDIM;
	  if (temp > MULTP * MAXP) temp = OVERFL;
	  if ((temp < -MULTP * MAXP) && (temp != UNKNDIM)) temp = -OVERFL;
	}
      this->dims[k] = temp;
    } // end of loop over 5 dimensions
  return (*this);
}

// add dimens entry checking for unknown
DIMEXP dimens::addem(const int a, const int b) const  
{
  if ((a == INCONS) || (b == INCONS)) return INCONS;
  if ((a == UNKNDIM) || (b == UNKNDIM)) return UNKNDIM;
  if (a+b != 0) {
    if ((a == OVERFL) ||(b == OVERFL)) return(OVERFL);
    if ((a == -OVERFL) ||(b == -OVERFL)) return(-OVERFL); 
  }
  else if ((a == OVERFL) ||(b == OVERFL)) return(UNKNDIM);
  if (((a == MAYBZ) && ((b == 0)) || (b == MAYBZ)) ||
      ((a == 0) && (b == MAYBZ))) return(MAYBZ);
  int temp = a+b;
  if (temp > MULTP * MAXP) temp = OVERFL;
  if (temp < -MULTP * MAXP) temp = -OVERFL;
  return((DIMEXP) temp);
}

dimens dimens::operator+(const dimens b) const
{
  dimens retdim;
  for (int k = 0; k < 5; k++)
    retdim.dims[k] = retdim.addem(dims[k],b.dims[k]);
  return(retdim);
}

dimens& dimens::operator+=(const dimens a)
{
  for (int k = 0; k < 5; k++)
    this->dims[k] = addem(a.dims[k],this->dims[k]);
  return(*this);
}

string dimens::print() 
{
  char dimsstr[25];
  if(unknp())
    sprintf(dimsstr,"(unknown)");
  else if(inconsp())
    sprintf(dimsstr,"(inconsistent");
  else
    sprintf(dimsstr,"(%4.1lf,%4.1lf,%4.1lf,%4.1lf,%4.1lf)",
	    getlengthd(),getmassd(),gettimed(),getcharged(),gettempd());
  return(string(dimsstr));
}
