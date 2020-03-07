#include <Rcpp.h>

using namespace Rcpp;


class Node{
public:
  NumericVector point;
  IntegerVector point_class;
  float med;
  int column;
  Node *left_node;
  Node *right_node;
public:
  Node() {};
  Node(int column, float med, NumericVector point, IntegerVector point_class, Node *left_node, Node *right_node);
  ~Node() {};
};
