#include <iostream>
#include <vector>
#include <string>
#include "toNNF.h"

using namespace std;

void toNNF(vector<string>& tokens, vector<string>& NNF_tokens) {

  
  auto begin = tokens.begin();
  auto end = tokens.end();

  while ( begin != end ) {

    if ( *begin == "imply" ) {

    } 
