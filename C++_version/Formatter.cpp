#include <vector>
#include <string>
#include <cctype>
#include "Formatter.h"
#include "data_type.h"

using namespace std;


// Utility Function

bool is_binary_connective(string& s) {

  return find(binary_connectives.begin(),
	      binary_connectives.end(),
	      s) != binary_connectives.end();

}

bool is_unary_connective(string& s) {

  return find(unary_connectives.begin(),
	      unary_connectives.end();
	      s) != unary_connectives.end();

}


// UNTESTED
// Given the raw input formula string and a vector<string> reference
// put tokens into the reference
// if successful, return true
// if contain unknown character(futher parse error in the future), return false
bool format(string& s, vector<string>& tokens) {
  
  auto begin = s.begin();
  auto end = s.end();

  while ( begin != end ) {

    string temp_s;
    
    if ( *begin == '(' || *begin == ')' ) {

      temp_s.push_back(*begin);

      begin++;
      
    } else if ( isspace(*begin) ) {
      
      ;
      
    } else {
      
    while ( *begin != '(' && *begin != ')' && !isspace(*begin) ) {
      
      temp_s.push_back(*begin);

      begin++;
      
    }

    tokens.push_back(temp_s);
    
    }
  }

  return true;
  
}


// Given a vector<string> reference and a pointer to node
// Return the root of the builded tree.
// Assuming that all the tokens are valid.
Node* build_tree(vector<string>& tokens, Node* root) {

  auto begin = tokens.begin();
  auto end = tokens.end();

  Node* cur = root;

  while ( begin != end ) {

    if ( *begin == "(" ) {

      auto old_cur = cur;
      cur->left = new Node();
      cur->right = new Node();
      cur = cur->left;
      cur->parent = old_cur;

    } else if ( *begin == ")" ) {

      cur = cur->parent;

    } else if ( is_binary_connective(*begin) ) {
      
      auto old_cur = cur;
      cur->id = *begin;
      cur = cur->right;
      cur->parent = old_cur;

    } else if ( is_unary_connective(*begin) ) {
      
      cur->id = *begin;

    } else {

      cur->id = *begin;
      cur = cur->parent;

    }

    begin++;
    
  }

  while ( root->parent != nullptr ) {
    
    root = root->parent;

  }

  return root;
  
}
      


void toNNF(Node* root) {

  if ( root->id == "imply" ) {

    convert_imply_equivalence(root);

  } else if ( root->id == "iff" ) {

    convert_iff_equivalence(root);

  } else if ( root->id == "not" ) {

    if ( root->left != nullptr ) {

      if ( root->left->id == "not" ) {

	// this part identifying the nested not
	// is not correct

	convert_not_not_equivalence(root);

      }
      
    }

    if ( root->right != nullptr ) {

      if (root->right->id == "not" ) {

	convert_not_not_equivalence(root);

      }

    }

  }
  

  if ( root->left != nullptr) {

    toNNF(root->left);
    
  }

  if ( root->right != nullptr ) {

    toNNF(root->right);

  }

}


// Given the node containining the imply connective
// change the imply formula to equivalence form
// return the new node i.e. the or connective
Node* convert_imply_equivalence(Node* root) {

  auto old_root = root;

  Node* new_root = new Node();

  new_root->id = "or";

  Node* not_node = new Node();

  not_node->id = "not";

  if ( root->parent != nullptr ) {

    new_root->parent = root->parent;

    if ( root->parent->left == root ) {

      new_root = root->parent->left;

    } else if ( root->parent->right == root ) {

      new_root = root->parent->right;

    }
    
  }

  new_root->left = not_node;

  not_node->parent = new_root;

  if ( root->left != nullptr ) {

    not_node->left = root->left;

    root->left->parent = node_node;

  }

  if ( root->right != nullptr ) {

    new_root->right = root->right;

    root->right = new_root;

  }

  return new_root;

}

// Given a subtree with node "not"
// i.e. the subtree contains another not node
// returns the reduced subtree
Node* convert_imply_not_not_equivalence(Node* root) {

  Node* not_node1 = new Node();

  not_node->id = "not";

  Node* not_node2 = new Node();

  not_node2->id = "not";
   
  if ( root->left != nullptr ) {

    if ( root->left->id == "not" ) {

      if ( root->parent != nullptr ) {

	root->parent->left = root->left->left;

	root->left->left->parent = root->parent;

      } else {

	root = root->left->left;

	root->parent = nullptr;

      }

    } else {

      // Apply de-Moivre's law in this region

      if ( root->left->id == "and" ) {

	root->left->id = "or";

      } else if ( root->left->id == "or" ) {

	root->right->id = "and";

      }

      // Assuming that the left side and
      // the right side of the connective
      // is not null

      // the order of these operations cannot be modified
      // to interpret them
      // draw a diagram


      // handle the left hand side of the connective
	
      root->left->left->parent = not_node1;

      not_node1->left = root->left->left;

      not_node1->parent = root->left;

      root->left->left = not_node1;

      // handle the right hand side of the connective
      
      root->left->right->parent = not_node2;

      not_node2->left = root->left->right;

      not_node2->parent = root->right;

      root->left->right = not_node2;

    }


}

void convert_iff_equivalence(Node* root) {

  // Assuming the left side and the
  // right side of the root is
  // not empty

  Node* and_node1 = new Node();

  and_node1->id = "and";

  Node* and_node2 = new Node();

  and_node2->id = "and";

  Node* not_node1 = new Node();

  not_node1->id = "not";

  Node* not_node2 = new Node();

  not_node2->id = "not";

  // Warning!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  // Need to figure out a way to make copies of tree
  Node* copy_a_1 = new Node();

  copy_create(root->left, copy_a_1);

  Node* copy_b_1 = new Node();

  copy_create(root->right, copy_b_1);

  Node* copy_a_2 = new Node();

  copy_create(root->left, copy_a_2);

  Node* copy_b_2 = new Node();

  copy_create(root->right, copy_b_2);

  // left branch of the equivalent form
  
  and_node1->left = copy_a_1;

  and_node1->right = copy_b_1;

  and_node1->left->parent = and_node1;

  and_node1->right->parent = and_node1;

  and_node1->parent = root;

  root->left = and_node1;

  // right branch of the equivalent form

  not_node1->left = copy_a_2;

  not_node2->left = copy_b_2;

  not_node1->left->parent = not_node1;

  not_node2->left->parent = not_node2;

  and_node2->left = not_node1;

  and_node2->right = not_node2;

  not_node1->parent = and_node2;

  not_node2->parent = and_node2;

  and_node2->parent = root;

  root->right = and_node2;

  root->id = "or";

}
}

// The function follows the following abstraction.
// (f1 and f2) or f3
// after the process:
// (f1 or f3) or (f2 or f3)
void distribute_disjunction_over_conjunction1(Node* root) {

  Node* new_f3 = new Node();

  copy_create(new_f3, root->right);

  Node* or_node = new Node();

  or_node->id = "or";

  // add the newly created or node to the right side

  or_node->parent = root;

  or_node->right = root->right;

  or_node->right->parent = or_node;

  root->right = or_node;

  // move the f2 to the left side

  or_node->left = root->left->right;

  or_node->left->parent = or_node;

  // move the newly created f3 to the left side

  root->left->right = new_f3;

  root->left->right->parent = root->left;
 
   
}

void distribute_disjunction_over_conjunction2(Node* root) {

  Node* new_f1 = new Node();

  copy_create(new_f1, root->left);

  Node* or_node = new Node();

  or_node->id = "or";

  // add the newly created or node to the left side

  or_node->left = root->left;

  or_node->left->parent = or_node;

  root->left = or_node;

  or_node->parent = root;

  // move f2 to the left side of the root

  or_node->right = root->right->left;

  or_node->right->parent = or_node;

  // move the newly created f1 to the right side

  root->right->left = new_f1;

  root->right->left->parent = root->right;

}


bool is_literal(Node* node) {

  // Assuming that the tree is constructed in the correct form

  if ( node->id == "not" ) {

    if ( node->left->left != nullptr || node->left->right != nullptr ) {

      return false;

    } else {

      return true;

    }
    
  } else if ( node->left == nullptr && node->right == nullptr ) {

    return true;

  } else {

    return false;

  }

}

void BCP(Node* root) {

  // Assuming that the tree is built in the correct format

  if ( root->id == "and" ) {

    if ( is_literal(root->left) ) {

      if ( root->right->id == "or" ) {
	


    } else if ( is_literal(root->right) ) {

	

	


    }


  }
  }
}

// Find the opposite of a given literal in a tree
// If the literal exist, return the node of the literal
// Otherwise, return a nullptr
// Note: the node returned is the one with the same id
// as the given one, to find whether it is in its negated form
// or not, look into its parent(a connective)
// NOTE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Only done the base case, haven't done the case where
// there maybe infinitely many opposite clauses in the literal
// Or, on second thought, they may be cancelled during recursion?????????
Node* find_oppposite(Node* root, bool b, string& id) {

  // the current literal is in normal form
  // i.e. it is not negated in the formula
  if (b) {

    if ( root->left == nullptr && root->right == nullptr ) {

      if ( root->id == id && root->parent->id == "not" ) {

	return root;
	
      } else {

	return nullptr;

      }

    } else {

      // Assuming that both clauses that form the conjunction
      // are literals that are formed by disjunction

      // At this stage, we only need to find one opposite

      Node* from_left = find_opposite(root->left, b, id);
      Node* from_right = find_opposite(root->right, b, id);

      if ( from_left != nullptr ) {

	return from_left;

      }

      if ( from_right != nullptr ) {

	return from_right;

      }

      return nullptr;

    }
	

  } else {

    if ( root->left == nullptr && root->right == nullptr ) {

      if ( root->id == id && root->parent->id != "not" ) {

	return root;

      } else {

	return nullptr;

      }
      
    } else {

      Node* from_left = find_opposite(root->left, b, id);
      Node* from_right = find_opposite(root->right, b, id);

      if ( from_left != nullptr ) {

	return from_left;

      }

      if ( from_right != nullptr ) {

	return from_right;

      }

      return nullptr;

  }
    
}
