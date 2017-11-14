#include <iostream>
#include <vector>
#include <string>

using namespace std;

string binary_connectives = { "and",
	              	       "or",
		            "imply",
		              "iff" };

string unary_connectives = { "not"};
		       

class Node {
 public:
  Node() = default;
  Node(Node*, Node*);
  Node* left = nullptr;
  Node* right = nullptr;
  string id = "";
 private:

};
  
  
void copy_create(Node*, Node*);
