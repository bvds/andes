//
//  Test program to convert from temporary test file
//  to a stream... see nonsense in ../src/coldriver.cpp
//
#include <iostream>
#include <sstream>
using namespace std;
#include "../src/decl.h"

std::istringstream inFile;

void pri(void){
  std::ostream outFile2 (inFile.rdbuf ()); 
  outFile2 << "and this" << endl;
}  

int main(int argc, char* argv[]) {
  std::istringstream inFile (std::ios::in | std::ios::out);
  std::ostream outFile (inFile.rdbuf ()); 
  //   ostringstream outFile(ss);
   outFile << "input this" << endl;
   pri();
  //    inFile.open(std::ios::in);// | std::ios::binary);
   //    inFile.clear();
   //  inFile.seekg(0);
  //   cout << "ss is now" << ss << endl;
   // istringstream inFile1(ss);
 cout << "hi" <<endl;
 cout << getaline(inFile)<<endl;
}
