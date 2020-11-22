#include <Rcpp.h>

using namespace Rcpp;

#define  RADIUS  6371
#define abs(x)  ( (x<0) ? -x : x )


double Distance(double long1, double lat1, double long2, double lat2) {
    double deg2radMultiplier = PI / 180;
    lat1 = lat1 * deg2radMultiplier;
    long1 = long1 * deg2radMultiplier;
    lat2 = lat2 * deg2radMultiplier;
    long2 = long2 * deg2radMultiplier;
 
    double radius = 6371.0090667; // earth mean radius defined by IUGG
    double dlong = long2 - long1;
    double distance = acos( sin(lat1) * sin(lat2) +  cos(lat1) * cos(lat2) * cos(dlong)) * radius; 
    return (distance);
}

// [[Rcpp::export]]
NumericVector distFunc2(NumericVector long1,
		       NumericVector lat1,
		       NumericVector long2,
		       NumericVector lat2,
		       int N){

  NumericVector ret(N);
   double d0;
   double lg1,lt1,lg2,lt2;
   for(int i=0;i<N;i++){
     lg1= long1[i] * PI/180;
     lt1= lat1[i] * PI/180;
     lg2= long2[i] * PI/180;
     lt2= lat2[i] * PI/180;
     if(lg1==lg2 && lt1==lt2)
       ret[i]=0;
     d0=sin(lt2)*sin(lt1)+cos(lt2)*cos(lt1)*cos((lg1-lg2));
     //d0=sin(lt2)*sin(lt1)+cos(lt2)*cos(lt1)*cos(abs(lg1-lg2));     
     ret[i]=d0>1?0:acos(d0)*RADIUS;
   }
   return ret;
}

// [[Rcpp::export]]
NumericVector distFunc(NumericVector long1,
		       NumericVector lat1,
		       NumericVector long2,
		       NumericVector lat2,
		       int N){

  NumericVector ret(N);
  for(int i=0;i<N;i++){
    ret[i]=Distance(long1[i],lat1[i],long2[i],lat2[i]);
  }
  return ret;
}
 
 
 
 // [[Rcpp::export]]
 double distFuncSingle(double long1,
                         double lat1,
                         double long2,
                         double lat2){
   return Distance(long1,lat1,long2,lat2);
 }



// [[Rcpp::export]]
NumericVector distFuncOneMany(double long1,
                       double lat1,
                       NumericVector long2,
                       NumericVector lat2,
                       int N){
  
  NumericVector ret(N);
  for(int i=0;i<N;i++){
    ret[i]=Distance(long1,lat1,long2[i],lat2[i]);
  }
  return ret;
}
