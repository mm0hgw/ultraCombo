#include <Rcpp.h>
using namespace Rcpp;

// based on code from Ben Voight
// http://stackoverflow.com/users/103167/ben-voigt
// http://stackoverflow.com/questions/9330915/number-of-combinations-n-choose-r-in-c

// [[Rcpp::export]]
IntegerVector combnGenElemRcpp(
	NumericVector xv,
	IntegerVector nv,
	IntegerVector kv,
	NumericVector chv
){
		// set up loop variables
	register double xr=xv[0], ch, oldch=chv[0];
	int n=nv[0], k=kv[0];
	register int i=n, j=k;
		// output vector
	IntegerVector out(k);
		// iterate until k-1 elements are chosen
	while(j>1){
			// iterate until the next element should be chosen
		while(xr > (ch = (oldch * j) / i)){
			xr -= ch;
			oldch -= ch;
			i--;
		}
			// choose an element
		out[k-j] = n-i+1;
		oldch = ch;
		i--;
		j--;
	}
		// choose last element
	out[k-1] = n-i+(int)xr;
	return(out);
}

// [[Rcpp::export]]
NumericVector revCombnGenElemRcpp(
	IntegerVector xv,
	IntegerVector nv,
	NumericVector chv
){
	int n=nv[0], k=xv.size();
  register int i=1, j=1, xr=xv[k-1]-xv[k-2];
  register int ch=chv[0]*k/n;
	NumericVector out(1);

	while(k-i>0){
		if(xv[i-1]-j==0){
			ch = ch * (k-i) / (n-j);
			i++;
			j++;
		}else{
			xr += ch;
			ch = ch - ch * (k-i) / (n-j);
			j++;
		}
	}
  out[0] = xr;
 	return out;
}
