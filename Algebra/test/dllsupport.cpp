//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
#define LRDC_MAIN
#include "lrdcstd.h"
#include <string>
#include <vector>

int dbgnum = 0;

#define _H_STANDARD_H_
#define LZ_EXTERN_SPEC LRDC_EXTERN_SPEC
#define LZ_INIT_INT_SPEC LRDC_INIT_INT_SPEC
#define LZ_INIT_PTR_SPEC LRDC_INIT_PTR_SPEC
#define LZ_SAFE_DELETE LRDC_SAFE_DELETE
#include "../src/extstruct.h"

bool getCanonEqn(std::string bufst);
bool getavar(std::string bufst);
int getavarwu(const string bufst,const bool varNew, const bool valToo, double & SIval);
bool makenn(std::string bufst);
bool makepos(std::string bufst);
bool makenz(std::string bufst);
bool makepar(std::string bufst);

typedef int varindx;

bool startsWith(std::string& aLine, std::string str) {
  if (str.length() < aLine.length()) {
		string tmp = aLine.substr(0, str.length());
		for (int k=0; k<tmp.length(); k++) {
			tmp[k] = tolower(tmp[k]);
		}
		return (tmp == str) ? true : false;
	}

  return false;
}

void removeBars(std::string& aLine) {
	int i, j;
	for (i=0, j=0; i<aLine.length(); i++) {
		if (aLine[i] != '|') {
			aLine[j++] = aLine[i];
		}
	}
	aLine[j] = '\0';
}

static bool isFirst = true;

#include "../src/unitabr.h"
extern unitabrs unittable;
void constsfill();

//////////////////////////////////////////////////////////////////////////////
bool handleInput(std::string& aLine) {
	bool result = true;

	if (! canoneqf) {
		canoneqf = new vector<binopexp*>; // ugly globals
	}
	if (! canonvars) {
		canonvars = new vector<physvar*>;
	}
	if (isFirst) {
		unittable.fill();
		constsfill();
		isFirst = false;
	}
	//removeBars(aLine);
	try {
		if (aLine.empty()) {
		} else if (startsWith(aLine, "(=")) {
			getCanonEqn(aLine);
		} else if (startsWith(aLine, "(variable")) {
			getavar(aLine);
		} else if (startsWith(aLine, "(svar")) {
			double jnk;
			getavarwu(aLine, true, false, jnk);
		} else if (startsWith(aLine, "(nonnegative")) {
			makenn(aLine);
		} else if (startsWith(aLine, "(positive")) {
			makepos(aLine);
		} else if (startsWith(aLine, "(nonzero")) {
			makenz(aLine);
		} else if (startsWith(aLine, "(parameter")) {
			makepar(aLine);
		} else if (startsWith(aLine, "<")) {
		} else if (startsWith(aLine, " ")) {
		} else if (startsWith(aLine, "\t")) {
		} else {
			throw std::string("Command: " + aLine + " not detected in handleInput");
			result = false;
		}
	} catch (std::string message) {
		throw message;
	} catch (...) {
		throw std::string("Handle Input is broke");
	}
	//throw(aLine); // this is how I found out about leading space and case issue (lht)
	return result; // Unexpected input
}

///////////////////////////////////////////////////////////////////////////////
// globalHolder of solution that was written to file stream maple

///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
bool clearTheProblem() {
	LZ_SAFE_DELETE(canonvars);
	LZ_SAFE_DELETE(canoneqf);
//	LZ_SAFE_DELETE(studeqf);
	LZ_SAFE_DELETE(solsexpr);
//	LZ_SAFE_DELETE(numsols);
	isFirst = true;
	return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
