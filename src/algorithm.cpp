#include "node.hpp"

using namespace Rcpp;

double euclidean_dist(NumericVector a, NumericVector b){
  if (a.size() != b.size()){
    Rcpp::stop("Vectors are not same length");
  }
  return sqrt(sum(pow(a-b, 2)));
}

Node *create_tree(NumericMatrix X, IntegerVector Classes, int depth = 0) {
  depth = depth % X.ncol();

  // Debugging
  if( depth < 0 ){
    Rcpp::stop("depth less than 0");
  } else if( depth >= X.ncol()){
    Rcpp::stop("depth greater than or equal to number of columns");
  }

  if(X.nrow() == 2){
    NumericVector col = X(_, depth);
    unsigned int index = (col(0) < col(1)) ? 0 : 1;

    double med = col(index);
    NumericVector median_point = X(index, _);

    int other_class = Classes(1-index);
    Node * other_side = new Node(depth,
                           X(1-index, depth),
                           X(1-index, _),
                           other_class,
                           NULL,
                           NULL);
    if (other_side->point.size() != X.ncol()){
      Rcpp::stop("Number of elements in node point do not equal number of points in training matrix");
    }
    Node * left_side = NULL;
    Node * right_side = NULL;

    // assign to correct node
    if(index == 1){
      left_side = other_side;
    } else {
      right_side = other_side;
    }

    Node * node = new Node(depth,
                           med,
                           median_point,
                           Classes(index),
                           left_side,
                           right_side);
    return node;
  } else if(X.nrow() == 1){
    Node * node = new Node(depth,
                           X(0, depth),
                                   X(0, _),
                                   Classes(0),
                                   NULL,
                                   NULL);
    if (node->point.size() != X.ncol()){
      Rcpp::stop("Number of elements in node point do not equal number of points in training matrix");
    }
    return node;
  } else if (X.nrow() == 0){
    return NULL;
  }

  NumericVector col = X(_, depth);
  col = clone(col).sort();

  double med = col(X.nrow() / 2);

  int n_left = sum(X(_, depth) < med);
  int n_right = sum(X(_, depth) > med);
  NumericMatrix left_side(n_left, X.ncol());
  IntegerVector left_classes(n_left);
  NumericMatrix right_side(n_right, X.ncol());
  IntegerVector right_classes(n_right);
  NumericVector median_point;
  int median_class;

  // Divide up the two nodes
  int count_l = 0;
  int count_r = 0;
  for(int i = 0; i < X.nrow(); i++){
    if(X(i, depth) < med){
      left_side(count_l, _) = X(i, _);
      left_classes(count_l++) = Classes(i);
    } else if (X(i, depth) > med){
      right_side(count_r, _) = X(i, _);
      right_classes(count_r++) = Classes(i);
    } else {
      median_point = X(i, _);
      median_class = Classes(i);
    }
  }
  Node *left_node = create_tree(left_side,
                                left_classes,
                                depth + 1);
  if (left_node->point.size() != X.ncol()){
    Rcpp::stop("Number of elements in left node point do not equal number of points in training matrix");
  }
  Node *right_node = create_tree(right_side,
                                 right_classes,
                                 depth + 1);

  if (right_node->point.size() != X.ncol()){
    Rcpp::stop("Number of elements in right node point do not equal number of points in training matrix");
  }
  Node *node = new Node(depth,
                        med,
                        median_point,
                        median_class,
                        left_node,
                        right_node);
  if (node->point.size() != X.ncol()){
    Rcpp::stop("Number of elements in node point do not equal number of points in training matrix");
  }
  return node;
}

struct Point{
  double dist;
  int point_class;
  NumericVector point;
  Point(){dist=-1.0; point_class=0L; point=NumericVector(1);};
  Point(double d, int pc, NumericVector v){
    this->dist = d;
    this->point_class = pc;
    this->point = v;
  };
};

Point find_neighbor(Node *tree, NumericVector test, Point best){

  if(!tree){
    return best;
  }

  if (tree->column > test.size()){
    Rcpp::stop("Column greater than number of elements");
  }
  if(test(tree->column) < tree->med){
    best = find_neighbor(tree->left_node, test, best);
  } else {
    best = find_neighbor(tree->right_node, test, best);
  }

  if(best.dist < 0 || euclidean_dist(tree->point, test) < best.dist){
    best.point = tree->point;
    best.dist = euclidean_dist(tree->point, test);
    best.point_class = tree->point_class;

  }
  return best;

}

//' Nearest Neighbor Classification
//'
//' Finds the closest neighbor for each point in the test matrix
//' and returns a vector of classifications
//'
//' @param train A numeric matrix of training points
//' @param test A numeric matrix of test points
//' @param classes A integer vector of training point classifications
//' @export
// [[Rcpp::export]]
IntegerVector nn_classification_cpp(NumericMatrix train, NumericMatrix test, IntegerVector classes){
  Node * tree = create_tree(train, classes);
  IntegerVector preds(test.nrow());

  for(int i=0; i < test.nrow(); i++){
    preds(i) = find_neighbor(tree, test(i, _), Point()).point_class;
  }
  return preds;
}

