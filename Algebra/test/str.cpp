//
//  Test program to convert from temporary test file
//  to a stream... see nonsense in ../src/coldriver.cpp
//
#include <iostream>
#include <sstream>
using namespace std;

stringstream inFile;
//ostringstream outFile;

void prim(void){
  // outFile << "output this" << endl;
  inFile << "output this" << endl;
  cout << "inFile.str:  "<<inFile.str();
  cout << "position:  "<<inFile.tellg()<<endl;
}  

void seco(void){
  cout << "position:  "<<inFile.tellg()<<endl;
  cout << inFile.str()<<endl;
}  

int main(int argc, char* argv[]) {
  prim();
  cout << "MinFile.str:  "<<inFile.str();
  cout << "Mposition:  "<<inFile.tellg()<<endl;
  seco();
}
