#include "node.hpp"
using namespace Rcpp;

Node::Node(int column, double med, NumericVector point, int point_class, Node *left_node, Node *right_node) {
  this->column = column;
  this->med = med;
  this->point = point;
  this->point_class = point_class;
  this->left_node = left_node;
  this->right_node = right_node;
}

void Node::print(int depth){
  for(int i = 0; i < depth; i++){
    Rcout << "\t";
  }
  Rcout << "column = " << column << " med = " << med << " class = " << point_class << "\n";
  if(!left_node){
    for(int i = 0; i < depth; i++){
      Rcout << "\t";
    }
    Rcout << "\t NULL\n";
  } else {
    left_node->print(depth + 1);
  }

  if(!right_node){
    for(int i = 0; i < depth; i++){
      Rcout << "\t";
    }
    Rcout << "\t NULL\n";
  } else {
    right_node->print(depth + 1);
  }

}
