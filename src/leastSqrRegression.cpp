
#include <Rcpp.h>
#include <stdio.h>
#include <cmath>
using namespace std;
using namespace Rcpp;


//[[Rcpp::export]]
List leastSqrRegression(NumericVector  x_in, NumericVector  y_in, IntegerMatrix Comb, bool intercept_zero = false){

  double SUMx, SUMy, SUMxy, SUMxx, SUMres, slope, y_intercept, SUM_Yres, AVGy, AVGx, Yres, Rsqr, res;
  NumericVector x,y;

  //Build the combinations
  int nrowz = Comb.nrow();
  int ncolz = (Comb.ncol()-1);
  NumericMatrix M_s(nrowz, ncolz);
  NumericMatrix M_i(nrowz, ncolz);
  NumericMatrix M_r(nrowz, ncolz);


  for (int rowz = 0; rowz < nrowz; rowz++){
    for (int colz = 0; colz < ncolz; colz++){
      SUMx=SUMy=SUMxy=SUMxx=SUMres=slope=y_intercept=SUM_Yres=AVGy=AVGx=0;

      NumericVector x = x_in[Rcpp::Range(Comb(rowz,colz), (Comb(rowz,(colz+1))-1))];
      NumericVector y = y_in[Rcpp::Range(Comb(rowz,colz), (Comb(rowz,(colz+1))-1))];
      int x_len = x.length();


      for (int i = 0; i < x_len; i++){
        SUMx = SUMx + x[i];
        SUMy = SUMy + y[i];
        SUMxy = SUMxy + x[i] * y[i];
        SUMxx = SUMxx + x[i] * x[i];
      }

      //calculate the means of x and y
      AVGy = SUMy / x_len;
      AVGx = SUMx / x_len;

      //slope or a1
      if(intercept_zero){
        slope = SUMxy / SUMxx;
        y_intercept = 0;
      }else{
        slope = (x_len * SUMxy - SUMx * SUMy) / (x_len * SUMxx - SUMx*SUMx);
        y_intercept = AVGy - slope * AVGx;
      }


      //calculate squared residues, their sum etc.
      double y_sim_sum = 0;
      double y_obs_sum = 0;

      for (int i = 0; i < x_len; i++){
        Yres = pow((y[i] - y_intercept - (slope * x[i])), 2);
        SUM_Yres += Yres;
        res = pow(y[i] - AVGy, 2);
        SUMres += res;
        y_sim_sum += pow(slope * x[i], 2);
        y_obs_sum += pow(y[i], 2);
      }

      if(intercept_zero){
        Rsqr = y_sim_sum / y_obs_sum;
      }else{
        Rsqr = (SUMres - SUM_Yres) / SUMres;
      }

      M_s(rowz,colz) = y_intercept;
      M_i(rowz,colz) = slope;
      M_r(rowz,colz) = Rsqr;
    }
  }

  List L = List::create(M_s, M_i, M_r);
  return(L);
}
