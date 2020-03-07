#include "node.hpp"

using namespace Rcpp;

const double TOL = .0000001;

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
    col = clone(col).sort();
    unsigned int index = (col(0) < col(1)) ? 0 : 1;

    double med = col(index);
    NumericVector median_point = X(index, _);

    Node * other_side = new Node(depth,
                           X(1-index, depth),
                           X(1-index, _),
                           Classes(1-index),
                           NULL,
                           NULL);
    if (other_side->point.size() != X.ncol()){
      Rcout << other_side->point.size() << " " << X.ncol() << "\n";
      Rcpp::stop("Number of elements in other node point do not equal number of points in training matrix2");
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
    Rcout << node->column << "\n";
    return node;
  } else if(X.nrow() == 1){
    Node * node = new Node(depth,
                           X(0, depth),
                                   X(0, _),
                                   Classes(0),
                                   NULL,
                                   NULL);
    if (node->point.size() != X.ncol()){
      Rcout << node->point.size() << " " << X.ncol() << "\n";
      Rcpp::stop("Number of elements in node point do not equal number of points in training matrix1");
    }
    return node;
  } else if (X.nrow() == 0){
    Rcout << "no rows\n";
    return NULL;
  }

  NumericVector col = X(_, depth);
  col = clone(col).sort();

  double med = col(X.nrow() / 2);
  Rcout << "med = " << med << "\n";

  int n_left = sum(X(_, depth) < med);
  int n_right = sum(X(_, depth) > med);
  Rcout << "nleft = " << n_left << " nright = " << n_right << "\n";
  NumericMatrix left_side(n_left, X.ncol());
  IntegerVector left_classes(n_left);
  NumericMatrix right_side(n_right, X.ncol());
  IntegerVector right_classes(n_right);
  NumericVector median_point;
  IntegerVector median_class;

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
      Rcout << "median size = " << median_point.size() << "\n";
    }
  }
  Rcout << "left = " << count_l << " right = " << count_r << "\n";
  Node *left_node = create_tree(left_side,
                                left_classes,
                                depth + 1);
  if (left_node->point.size() != X.ncol()){
    Rcout << left_node->point.size() << " " << X.ncol() << "\n";
    Rcpp::stop("Number of elements in left node point do not equal number of points in training matrix");
  }
  Node *right_node = create_tree(right_side,
                                 right_classes,
                                 depth + 1);

  if (right_node->point.size() != X.ncol()){
    Rcout << right_node->point.size() << " " << X.ncol() << "\n";
    Rcpp::stop("Number of elements in right node point do not equal number of points in training matrix");
  }
  Node *node = new Node(depth,
                        med,
                        median_point,
                        median_class,
                        left_node,
                        right_node);
  if (node->point.size() != X.ncol()){
    Rcout << node->point.size() << " " << X.ncol() << "\n";
    Rcpp::stop("Number of elements in node point do not equal number of points in training matrix");
  }
  return node;
}

struct Point{
  double dist;
  IntegerVector point_class;
  NumericVector point;
  Point(){dist=-1.0; point_class=0L; point=NumericVector(1);};
  Point(double d, int pc, NumericVector v){
    this->dist = d;
    this->point_class = pc;
    this->point = v;
  };
};

Point find_neighbor(Node *tree, NumericVector test, Point best, int depth2 = 0){
  depth2 = depth2 % test.size();
  Rcout << "depth = " << depth2 << "\n";
  if (depth2 > 100){
    Rcpp::stop("over 100");
  }
  Rcout << "\nEntering find_neighbor\n";
  if(!tree){
    Rcout << "NULL tree" << "\n";
    return best;
  }

  Rcout << "Test depth\n";
  Rcout << "median = " << tree->med << "\n";
  if (tree->column > test.size()){
    Rcout << "tree col = " << tree->column << "\n";
    Rcpp::stop("Column greater than number of elements");
  }
  Rcout << "Tree column = " << tree->column << " ncols = " << tree->point.size() << " test.n_elem = " << test.size() << "\n";
  if(test(tree->column) < tree->med){
    Rcout << "Left\n";
    best = find_neighbor(tree->left_node, test, best, ++depth2);
    Rcout << "leave left" << "\n";
  } else {
    Rcout << "Right\n";
    best = find_neighbor(tree->right_node, test, best, ++depth2);
    Rcout << "leave right" << "\n";
  }

  Rcout << "Test best\n";
  if(best.dist < 0 || euclidean_dist(tree->point, test) < best.dist){
    Rcout << "New  Best\n";
    Rcout << "point.n_elem = " << tree->point.size() << "\n";
    Rcout << "point.class = " << tree->point_class << "\n";
    Rcout << "set point" << "\n";
    best.point = tree->point;
    Rcout << "get dist" << "\n";
    best.dist = euclidean_dist(tree->point, test);
    Rcout << "set class" << "\n";
    best.point_class = tree->point_class;

  }
  Rcout << "Exit find neighborr\n\n";
  Rcout <<  "Best = " << best.dist << " class = " << best.point_class << "\n";
  return best;

}

// [[Rcpp::export]]
IntegerVector nn_classification_cpp(NumericMatrix train, NumericMatrix test, IntegerVector classes){
  //Rcout << "Entering classification_cpp\n";
  // arma::mat train = as<arma::mat>(t1);
  //  arma::mat test = as<arma::mat>(t2);
  //  arma::vec classes = as<arma::vec>(cl);
  Node * tree = create_tree(train, classes);

  IntegerVector preds(test.nrow());

  for(int i=0; i < test.nrow(); i++){
    preds(i) = find_neighbor(tree, test(i, _), Point()).point_class(0);
  }
  return preds;
}

