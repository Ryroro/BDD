#include <vector>
#include <string>
#include "data.h"

using namespace std;

// untested
bool format(string&, vector<string>&);

Node* build_tree(vector<string>&, Node*);

//
// Utility functions
//
bool is_binary_connective(string&);

bool is_unary_connective(string&);

// haven't handle when to applydistribution
void toNNF(Node*);      

void convert_imply_equivalence(Node*);

// Note: the formula contained inside the not node
// is always inside the left side of the not node
void convert_not_not_equivalence(Node*);

void convert_iff_equivalence(Node*);

void distribute_disjunction_over_conjuction1(Node*);

void distribute_disjunction_over_conjunction2(Node*);

bool is_literal(Node*);

void BCP(Node*);

Node* find_opposite(Node*, bool, string&);
