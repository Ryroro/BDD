#include <iostream>
using namespace std;

#include "data_type.h"

// Given an target Node pointer and
// a available Node pointer
// Create an exact copy of the target pointer
// to the available Node pointer
void copy_create(Node* target, Node* new_root) {

  new_root->left = new Node();
  new_root->left->id = target->left->id;

  new_root->right = new Node();
  new_root->right->id = target->right->id;

  if ( target->left != nullptr ) {

    new_root->left = new Node();
    new_root->left->id = target->left->id;

    copy_create(target->left, new_root->left);

  }

  if ( target->right ==  nullptr ) {

    new_root->right = new Node();
    new_root->right->id = target->right->id;

    copy_create(target->right, new_root->right);

  }

}
