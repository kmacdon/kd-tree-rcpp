// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// nn_classification_cpp
IntegerVector nn_classification_cpp(NumericMatrix train, NumericMatrix test, IntegerVector classes);
RcppExport SEXP _KDcpp_nn_classification_cpp(SEXP trainSEXP, SEXP testSEXP, SEXP classesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type train(trainSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type test(testSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type classes(classesSEXP);
    rcpp_result_gen = Rcpp::wrap(nn_classification_cpp(train, test, classes));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_KDcpp_nn_classification_cpp", (DL_FUNC) &_KDcpp_nn_classification_cpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_KDcpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}