#include <iostream>
#include <vector>
#include <string>

using namespace std;


// Encoding operation
// and : -1
// or : -2
// imply : -3
// iff : -4

class Encoder {
 public:
  Encoder();
  void encode(vector<string>&, vector<int>&);
 private:
  int cur_id;
};
