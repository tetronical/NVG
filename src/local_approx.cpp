#include <Rcpp.h>
using namespace Rcpp;


/**
 * Square each element of a numeric vector.
 *
 * @param data A numeric vector.
 * @param start_p Starting point of the data (after this point predictions start).
 * @param m Embedding dimension.
 * @param neighbors Number of neighbors to consider.
 * @return A numeric vector with each element squared.
 * @examples
 * local_approx(data = 1:10, start_p = 2, m = 2, neighbors = 1)
 * @export
 */


// [[Rcpp::export]]
NumericVector local_approx(NumericVector data,int start_p, int m, int neighbors) {
  int TotalData = data.length();
  int tao = 1; // Delay time
  NumericVector forecast(TotalData, NA_REAL);
  //Rcout << "Original Data: \n" <<  std::endl;
  //Rcpp::print(data);

  for (int i = start_p; i < TotalData; i++) {
    NumericVector sub_data(i);
    for (int j = 0; j < i; j++) {
      sub_data[j]=data[j];
    }
    //Rcout << "\n\t--------------\t"<< std::endl;

    //Rcout << "\nSub Data: 1 to " <<  sub_data.length() << "\n"<< std::endl;

    //Rcpp::print(sub_data);
    // Phase Space Recostruction begins
    int N = i;

    int M = N - (m - 1) * tao;

    NumericMatrix Y(M, m);
    // Phase Space Reconstruction

    for (int iok = 0; iok < m; iok++) {
      for (int jok = 0; jok < M; jok++) {
        Y(jok, m - iok - 1) = data[jok + iok * tao];
      }
    }

    //Rcout << "\n Reconstructed Data: \n" <<  std::endl;

    //Rcpp::print(Y);


    NumericVector Last_vector=Y(M-1,_);

    NumericVector distance(M);

    // distance calculations
    for (int furlong = 0; furlong < M; furlong++) {
      NumericVector dis_arr = pow((Y(furlong,_) - Last_vector),2);
      distance[furlong] = sqrt(std::accumulate(dis_arr.begin(),dis_arr.end(),0.0));
    }

    //Rcout << "\nDistances from Last Vector ("<< Last_vector <<") :\n" <<  std::endl;

    //Rcpp::print(distance);

    int n = distance.size();

    // Initialize an index vector
    IntegerVector indexes = seq_len(n) - 1;
    //Rcout << "\nUnsorted indices: \n" <<  std::endl;
    //Rcpp::print(indexes);

    // Sort the index vector based on the values in x
    std::sort(indexes.begin(), indexes.end(), [&](int pok, int loc) {
      return distance[pok] < distance[loc];
    });
    //Rcout << "\nSorted indices: \n" <<  std::endl;
    //Rcpp::print(indexes);

    //Rcout << "\nFirst value: " << indexes[0] << std::endl;


    double ans;

    if (neighbors == 1) {
      if (indexes[1] == M){
        //Rcout << "\n# of Neighbours: " << neighbors << std::endl;

        ans = Y((indexes[0]+1), 0); // indexes[1] is the minimum and +1 is the future
      } else {
        //Rcout << "\n# of Neighbours: " << neighbors << std::endl;
        //Rcout << "\nIndex of 1 neighbour: " << indexes[1] << std::endl;

        // 0 is the same value
        // 1 is the minimum distance
        // 2 is the future value
        ans = Y((indexes[1]+1), 0); // indexes[1] is the minimum and +1 is the future
      }

    } else {
      //Rcout << "\n# of Neighbours: " << neighbors << std::endl;

      int count = 0; // Counter variable

      NumericVector neighbor_values(neighbors + 1); // Predefined size for neighbor_values

      //Rcout << "\nCapturing the neighbours: " << neighbor_values << std::endl;

      for (int neib = 0; neib < (neighbors+1); neib++ ){

        //Rcout << "\nNo of neighbours captured: " << neib << std::endl;

        if (indexes[neib] != M-1) {
          //Rcout << "\nWhile Capturing:" <<indexes[neib]<<"\t future value is: "<<indexes[neib]+1 <<"\t and corresponding is: "<< Y((indexes[neib] + 1), 0)<< std::endl;
          neighbor_values[count] = Y((indexes[neib] + 1), 0);
          count++;
        }
      }
      neighbor_values = neighbor_values[seq_len(count)-1]; // Truncate neighbor_values to valid values
      //Rcout << "\nAfter capturing the neighbours: " << neighbor_values << std::endl;

      ans = mean(neighbor_values);
      //Rcout << "\nAnswer is the mean value: "<< ans <<"\n"<< std::endl;
    }
    forecast[i]=ans;
  }

  return forecast;
}
