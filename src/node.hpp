#include <Rcpp.h>

using namespace Rcpp;


class Node{
public:
  NumericVector point;
  int point_class;
  double med;
  int column;
  Node *left_node;
  Node *right_node;
  std::string creation;
public:
  Node() {};
  Node(int column, double med, NumericVector point, int point_class, Node *left_node, Node *right_node, std::string creation = "blank");
  ~Node() {};
  void print(int depth = 0);
};
