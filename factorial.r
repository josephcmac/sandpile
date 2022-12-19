library(Rcpp)

rm(list = ls())

factR <- function(n) {
  if (n == 0)
    1 
  else
    n*factR(n-1)
} 


cppFunction(
  'int factC(int n) {
      if (n == 0) {
        return 1;
      } else {
        return n * factC(n-1);
      }
    }')


system.time( sapply(0:500, factR)  )

system.time( sapply(0:500, factC)  )

