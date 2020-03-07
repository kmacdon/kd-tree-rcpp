#include "node.hpp"
using namespace Rcpp;

Node::Node(int column, float med, NumericVector point, IntegerVector point_class, Node *left_node, Node *right_node) {
  this->column = column;
  this->med = med;
  this->point = point;
  this->point_class = point_class;
  this->left_node = left_node;
  this->right_node = right_node;
}
