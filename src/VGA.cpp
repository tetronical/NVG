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

NumericMatrix VGA(NumericVector t) {
  int n=t.size();
  NumericMatrix adj_mat(n,n);
  double ya, yb, ta, tb, tc;
  for(int i=0; i<n; i++){
    for(int j=0; j<n;j++){
      ya=t[i];
      yb=t[j];
      ta=i;
      tb=j;
      adj_mat(i,j)=1;
      //Rcout << "i is: " << i+1<<  "["  << ya << "]" << "\t"<< "j is: " << j+1<<  "["  << yb << "]" << "\\n";
      //Rcout << "adj_mat[" << i+1<<","<< j+1  << "] = "  <<  adj_mat(i,j) <<"\\n";
      if(i>j){
        for (int k=j; k<i; k++){
          tc=k;
          //Rcout << "k is: =  " << k+1 << "\\n";
          if (k != i && k != j) {
            if(t[k]>(yb + (ya - yb) * ( (tb - tc) / (tb - ta) ))){
              /*Rcout << "k exit stat is: =  " << k+1 << "\\n";
               Rcout << "i is: " << i+1<<  "["  << ya << "]" << "\t"<< "j is: " << j+1<<  "["  << yb << "]" << "\\n";
               Rcout << " yc is: " << t[k] << "\\n";
               Rcout << (tb - tc) << "\t"<< (tb - ta) <<"\t" <<( (tb - tc) / (tb - ta) )<<"\\n";
               Rcout << " rhs is: " << (yb + ((ya - yb) * ( (tb - tc) / (tb - ta) ))) << "\\n";*/
              adj_mat(i,j) = 0;
              break;
            }
          }
        }
      }else {
        //Rcout << "i is: " << i+1<<  "["  << ya << "]" << "\t"<< "j is: " << j+1<<  "["  << yb << "]" << "\\n";
        for (int k=i; k<j; k++){
          tc=k;
          //Rcout << "k is outside: =  " << k+1 << "\\n";
          if (k != i && k != j) {
            //Rcout << "k is inside: =" <<k+1 <<"\t" << i+1 <<"\t"<< j+1 << "\\n";
            if(t[k]>(yb + (ya - yb) * ( (tb - tc) / (tb - ta) ))){
              /*Rcout << "k exit stat is: =  " << k+1 << "\\n";
               Rcout << "i is: " << i+1<<  "["  << ya << "]" << "\t"<< "j is: " << j+1<<  "["  << yb << "]" << "\\n";
               Rcout << " yc is: " << t[k] << "\\n";
               Rcout << (tb - tc) << "\t"<< (tb - ta) <<"\t" <<( (tb - tc) / (tb - ta) )<<"\\n";
               Rcout << " rhs is: " << (yb + ((ya - yb) * ( (tb - tc) / (tb - ta) ))) << "\\n";*/
              adj_mat(i,j) = 0;
              break;
            }
          }
        }
        //Rcout << "adj_mat[" << i+1<<","<< j+1  << "] = "  <<  adj_mat(i,j) <<"\\n";
      }//else statement
    }
  }
  return adj_mat;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
