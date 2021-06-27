#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

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
NumericMatrix vvga(NumericMatrix v){
  int n = v.nrow(),ta, tb, tc;
  NumericVector m=v[0];
  NumericMatrix adj_mat(n,n);
  NumericVector ya, yb, yc;
  for (int i=0; i<n; i++){
    ta = i;
    ya = v( i , _ );
    double Ya = sqrt(inner_product(ya.begin(), ya.end(), ya.begin(), 0.0));
      for(int j=i; j<n; j++){
        yb = v( j , _ );
        tb = j;
        double r1 = inner_product(ya.begin(), ya.end(), yb.begin(), 0.0);
        double Yab = r1/Ya;
        adj_mat(i,j)=1;
        Rcout<<"Yab is: " <<Yab << "\t" <<"Ya is: "<< Ya << "\n";
        Rcout<<" ta is: " << i+1 << "\t tb is: " << j+1 << "\n";
        for ( int k=i; k<j; k++){
          if (k != i && k != j){
            yc = v ( k , _);
            tc = k;
            double r2 = inner_product(ya.begin(), ya.end(), yc.begin(), 0.0);
            double Yac = r2/Ya;
            Rcout<<"Yac is: "<<Yac<< "\t tc is: " <<k+1<<"\n";
            if (Yac > Yab + ((Ya - Yab)*(tb-tc)/(tb-ta))){
              Rcout<<"Yac is: "<<Yac<< "\t tc is: " <<k+1<<"\n";
              adj_mat(i,j)=0;
              Rcout<< "adj_mat["<< i+1 <<","<<j+1 <<"] = "<< adj_mat(i,j)<<"\n";
              break;
              }
            }
          }
        //Rcout << "Inner product of a and b: " << r2 << '\n';
        }
    }
  return adj_mat;
  }

//NumericVector v (3,1);

//NumericVector v = {1,2,3}; 

//NumericVector v = NumericVector::create(1,2,3);

//NumericVector v = NumericVector::create(Named("x",1), Named("y")=2 , _["z"]=3);



