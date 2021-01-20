#include <Rcpp.h>
using namespace Rcpp;

// This is a simple edataample of edataporting a C++ function to R. THRou can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallerTHR.rcpp.org/
//

// [[Rcpp::export]]
NumericVector findprd(NumericVector data, double THR, int k) // 阈值THR 最少k区间
{
  int fp[10000][2], cc = 0;
  int n = data.size();
  int i = 0;
  
  NumericVector HW_status(n); // fill with zero
  
  if (n < k)
  {
    return HW_status;
  }
  while (i <= (n - k + 1))
  {
    int sum = 0, flag = 1, cnt = k;
    for (int j = 0; j < k; j++)
    {
      sum += data[j + i];
      if (data[j + i] <= THR)
      {
        flag = 0;
        break;
      }
    }
    if (flag == 1)
    {
      int j;
      for (j = i + k; j < n; j++)
      {
        if ((sum + data[j]) / (cnt + 1) <= THR)
          break;
        sum += data[j];
        cnt++;
      }
      fp[cc][0] = i;
      fp[cc++][1] = j;
      i = j + 1;
    }
    else
      i++;
  }
  
  for (int j = 0; j < cc; j++)
  {
    int i_begin = fp[j][0];
    int i_end = fp[j][1];
    for (int i = i_begin; i < i_end && i < n; i++)
      HW_status[i] = 1.0;
  }
  return HW_status;
}

// THRou can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automaticallTHR
// run after the compilation.
//