#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix phase_space(NumericVector x, int m, int tau) {

  int N=x.length();

  int M=N-((m-1)*tau);

  NumericMatrix Y(M,m);

  for(int i=0; i<m; i++){

    Y(_,i)=x[seq(0,M)+i*tau];//x[seq(1,M)+(i-1)*tau]
  }
  return Y;
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
phase_space(1:10,2,1)
*/
